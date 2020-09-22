import React, { useEffect } from 'react';
import BaseInfo from '../report-page/components/base-info';
import BlockList from '../report-page/components/block-list';
import styles from './index.less';
import TaskContext from './taskContext';
import { ITask } from './generateTask';

interface Props {
  innerRef?: React.Ref<HTMLDivElement>
  fullPage?: boolean
  task?: ITask
}
const PreviewReport: React.FC<Props> = ({ innerRef, task, fullPage = false }) => {
  if (fullPage) {
    if (!document.body.classList.contains('hidden')) {
      document.body.classList.add('hidden');
    }
  }
  useEffect(() => () => {
    document.body.classList.remove('hidden');
  }, []);

  return (
    <TaskContext.Provider value={task as ITask}>
      <div className={styles.preview} ref={innerRef}>
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
};

export default PreviewReport;
