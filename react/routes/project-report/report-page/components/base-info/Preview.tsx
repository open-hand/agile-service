import React, { useCallback, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { useTaskContext } from '@/routes/project-report/report-preview/taskContext';
import { useProjectReportContext } from '../../context';
import styles from './Preview.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const BaseInfoPreview: React.FC = () => {
  const { store } = useProjectReportContext();
  const formatMessage = useFormatMessage();

  const { baseInfo } = store;
  const { register, finish } = useTaskContext();
  register('baseInfo');
  const onFinish = useCallback(() => {
    finish('baseInfo');
  }, [finish]);
  useEffect(() => {
    onFinish();
  }, [onFinish]);
  return (
    <div className={`${styles.container} c7n-project-report-block`}>
      <div className={styles.title}>
        {baseInfo?.title}
      </div>
      <section>
        <div className={styles.label}>
          报告说明:
        </div>
        <div>
          {baseInfo?.description || '-'}
        </div>
      </section>
      <section>
        <div className={styles.label}>
          {formatMessage({ id: 'agile.projectReport.receiver' })}
          :
        </div>
        <div>
          {(baseInfo?.receiverList || []).map((user) => user.realName).join(',')}
        </div>
      </section>
      <section>
        <div className={styles.label}>
          抄送人:
        </div>
        <div>
          {(baseInfo?.ccList || []).map((user) => user.realName).join(',')}
        </div>
      </section>
      <div style={{
        height: 1,
        background: '#3F51B5FF',
        margin: '30px 0 20px 0',
      }}
      />
    </div>
  );
};
export default observer(BaseInfoPreview);
