import { useRef } from 'react';

export default function useAvoidClosure(fn: Function) {
  const ref = useRef(fn);
  ref.current = fn;
  const callback = (...args: any[]) => ref.current(...args);
  return callback;
}
