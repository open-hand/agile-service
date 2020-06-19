import { useRef, useCallback, useEffect } from 'react';

function useClickOut<T extends HTMLElement>(onClickOut: (event: MouseEvent) => void) {
  const ref = useRef<T>() as React.MutableRefObject<T>;
  const handleClick = useCallback((e: MouseEvent) => {
    if (ref.current && !ref.current.contains(e.target as HTMLElement)) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick);
    return () => {
      document.removeEventListener('click', handleClick);
    };
  }, [handleClick]);
  return ref;
}
export default useClickOut;
