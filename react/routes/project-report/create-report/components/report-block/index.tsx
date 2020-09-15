import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import styles from './index.less';
import { IReportBlock } from '../../store';
import TextBlock from './components/text-block';
import ChartBlock from './components/chart-block';

interface Props {
  data: IReportBlock

}
const BlockMap = new Map([
  ['text', TextBlock],
  ['chart', ChartBlock],
]);
const ReportBlock: React.FC<Props> = ({ data }) => {
  const { title, type } = data;
  const BlockComponent = BlockMap.get(type);
  return (
    <div className={styles.report_block}>
      <div className={styles.header}>
        <span className={styles.title}>{title}</span>
        <div className={styles.operation}>
          <Button icon="edit-o" color={'blue' as ButtonColor}>编辑</Button>
          <Button icon="delete" color={'blue' as ButtonColor}>删除</Button>
        </div>
      </div>
      {BlockComponent && <BlockComponent data={data} />}
    </div>
  );
};

export default ReportBlock;
