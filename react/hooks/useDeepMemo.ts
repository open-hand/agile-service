import { isEqual } from 'lodash';
import { useRef, useMemo } from 'react';

export default function useDeepMemo<T>(fn: () => T, deps?: any[]) {
  const valueRef = useRef<T>(fn());
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const newValue = useMemo(fn, deps);
  if (!isEqual(newValue, valueRef.current)) {
    valueRef.current = newValue;
  }
  return valueRef.current;
}
