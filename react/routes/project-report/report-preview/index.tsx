import React, { useEffect, useCallback } from 'react';
import { Button } from 'choerodon-ui/pro';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import BaseInfo from '../report-page/components/base-info';
import BlockList from '../report-page/components/block-list';
import styles from './index.less';
import { useProjectReportContext } from '../report-page/context';
import TaskContext from './taskContext';
import { ITask } from './generateTask';

interface Props {
  innerRef?: React.Ref<HTMLDivElement>
  fullPage?: boolean
  task?: ITask
}
const PreviewReport: React.FC<Props> = ({ innerRef, task, fullPage = false }) => {
  const { doExport } = useProjectReportContext();
  if (fullPage) {
    if (!document.body.classList.contains('hidden')) {
      document.body.classList.add('hidden');
    }
  }
  useEffect(() => () => {
    document.body.classList.remove('hidden');
  }, []);
  // const handleExportClick = useCallback(() => {
  //   doExport();
  // }, [doExport]);

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
      {/* <Button funcType={'raised' as FuncType} onClick={handleExportClick}>导出</Button> */}
    </TaskContext.Provider>
  );
};

export default PreviewReport;
