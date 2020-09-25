import React from 'react';
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
}
const PreviewReport: React.FC<Props> = ({
  innerRef, task, ...otherProps
}) => (
  <TaskContext.Provider value={task || {
    register: () => { },
    finish: () => { },
    reset: () => { },
  }}
  >
    <div className={styles.preview} ref={innerRef} {...otherProps}>
      <BaseInfo preview />
      <div style={{
        height: 1,
        background: '#3F51B5FF',
        margin: '30px 0 20px 0',
      }}
      />
      <div>
        <BlockList preview />
      </div>
    </div>
  </TaskContext.Provider>
);

export default PreviewReport;
