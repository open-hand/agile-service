import React, { useRef, useEffect } from 'react';
import ChartContext from '@/components/charts/context';
import BaseInfo from '../report-page/components/base-info';
import BlockList from '../report-page/components/block-list';
import styles from './index.less';
import TaskContext from './taskContext';
import { ITask } from './generateTask';

interface Props {
  innerRef?: React.Ref<HTMLDivElement>
  task?: ITask
  className?: string
  style?: React.CSSProperties
  scale?:number
}
const PreviewReport: React.FC<Props> = ({
  innerRef, task, scale, ...otherProps
}) => {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (scale && ref.current) {
      const elements = ref.current.querySelectorAll('*');
      for (let i = 0; i <= elements.length; i += 1) {
        const element = elements[i];
        if (element && !element.getAttribute('data-large')) {
          const { fontSize } = window.getComputedStyle(element);
          const originSize = Number(fontSize.split('px')[0]);
          element.setAttribute('data-large', 'true');
          if (originSize < 18) {
            // @ts-ignore
            element.style.fontSize = `${originSize * scale}px`;
          }
        }
      }
    }
  }, [scale]);
  return (
    <ChartContext.Provider value={{ scale: scale || 1 }}>
      <TaskContext.Provider value={task || {
        register: () => { },
        finish: () => { },
        reset: () => { },
      }}
      >
        <div className={styles.preview} ref={innerRef} {...otherProps}>
          <div ref={ref}>
            <BaseInfo preview />
            <div>
              <BlockList preview />
            </div>
          </div>
        </div>
      </TaskContext.Provider>
    </ChartContext.Provider>
  );
};

export default PreviewReport;
