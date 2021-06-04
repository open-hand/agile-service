import { useFullscreen as useFull, usePersistFn } from 'ahooks';

export default function useFullScreen(target, onFullScreenChange, customClassName = 'fullScreen') {
  const element = typeof target === 'function' ? target() : target;

  const handleExitFull = usePersistFn(() => {
    element.classList.remove(customClassName);
  });
  const handleFull = usePersistFn(() => {
    element.classList.add(customClassName);
  });
  const [isFullscreen, { toggleFull }] = useFull(target, {
    onFull: handleFull,
    onExitFull: handleExitFull,
  });

  return [isFullscreen, toggleFull];
}
