import React, {
  useContext, useCallback, useState, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import Context from '../../context';
import styles from './index.less';
import { Gantt } from '../../types';
import { ROW_HEIGHT } from '../../constants';

interface TaskBarProps {
  data: Gantt.Bar
}
const barH = 8;
let startX = 0;

const InvalidTaskBar: React.FC<TaskBarProps> = ({ data }) => {
  const { store } = useContext(Context);
  const triggerRef = useRef<HTMLDivElement>(null);
  const {
    translateY, translateX, width, dateTextFormat,
  } = data;
  const [visible, setVisible] = useState(false);
  const { translateX: viewTranslateX } = store;
  const top = translateY;
  const handleMouseEnter = useCallback(() => {
    if (store.gestureKeyPress) {
      return;
    }
    startX = triggerRef.current?.getBoundingClientRect()?.left || 0;
    setVisible(true);
  }, [store.gestureKeyPress]);
  const handleMouseLeave = useCallback(() => {
    if (store.gestureKeyPress) {
      return;
    }
    setVisible(false);
    store.handleInvalidBarLeave();
  }, [store]);
  const handleMouseMove = useCallback((event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
    event.stopPropagation();
    if (store.gestureKeyPress) {
      return;
    }
    // TODO 优化点击后移动的逻辑
    const pointerX = viewTranslateX + (event.clientX - startX);
    // eslint-disable-next-line no-shadow
    const { left, width } = store.startXRectBar(pointerX);
    // 移动的时候就不改translateX了
    if (data.stepGesture === 'moving') {
      store.handleInvalidBarMove(data, left, Math.ceil(width));
    } else {
      store.handleInvalidBarHover(data, left, Math.ceil(width));
    }
  }, [data, store, viewTranslateX]);
  const handleMouseDown = useCallback((event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
    event.stopPropagation();
    store.handleInvalidBarDown(data);
  }, [data, store]);
  const handleMouseUp = useCallback(() => {
    store.handleInvalidBarUp(data);
  }, [data, store]);
  return (
    <div
      role="none"
      onMouseEnter={handleMouseEnter}
      onMouseLeave={handleMouseLeave}
      onMouseMove={handleMouseMove}
      onMouseDown={handleMouseDown}
      onMouseUp={handleMouseUp}
    >
      <div
        ref={triggerRef}
        className={styles['task-row-trigger']}
        style={{
          left: viewTranslateX, transform: `translateY(${top - ((ROW_HEIGHT - barH) / 2)}px`,
        }}
      />
      {visible && (
        <div
          className={styles.block}
          aria-haspopup="true"
          aria-expanded="false"
          style={{
            left: translateX,
            width: Math.ceil(width),
            transform: `translateY(${top}px)`,
            backgroundColor: 'rgb(149, 221, 255)',
            borderColor: 'rgb(100, 199, 254)',
          }}
        >
          <div
            className={styles.date}
            style={{
              right: Math.ceil(width + 6),
            }}
          >
            {dateTextFormat(translateX)}
          </div>
          <div
            className={styles.date}
            style={{
              left: Math.ceil(width + 6),
            }}
          >
            {dateTextFormat(translateX + width)}
          </div>
        </div>
      )}
    </div>
  );
};
export default observer(InvalidTaskBar);
