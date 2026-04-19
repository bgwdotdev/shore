import { EventEmitter } from 'node:events';
import { Result$Ok, Result$Error } from '../gleam.mjs';

export function terminal_rows() {
  return Result$Ok(process.stdout.rows);
}

export function terminal_columns() {
  return Result$Ok(process.stdout.columns);
}

export function raw() {
  process.stdin.setRawMode(true);
  return undefined;
}

export function cooked() {
  process.stdin.setRawMode(false);
}

export function on_input(fun) {
  process.stdin.setEncoding("utf8");
  process.stdin.on("data", (key) => {
    fun(key);
  });
}


class Shore extends EventEmitter {
  #state;
  #loop;

  constructor({init, loop}) {
    super();
    const send = this.send.bind(this);
    this.#state = init(send);
    this.#loop = loop;

    this.on("event", (event) => {
      this.#state = this.#loop(this.#state, event);
    });
  }

  send(event) {
    this.emit("event", event);
  }

}

export function start(init, loop) {
  return new Shore({init, loop});
}

export function send(shore, event) {
  shore.send(event);
}

export function exit() {
  process.exit(0);
}

export function timeout(callback, duration) {
  return setTimeout(callback, duration);
}

export function f(url) {
  return fetch(url);
}

export function spawn(fun) {
  queueMicrotask(async () => { await fun() })
}

export function is_windows() {
  return process.platform === "win32";
}

export function sigwinch(fun) {
  process.stdout.on("resize", () => {
    fun()
  });
}

