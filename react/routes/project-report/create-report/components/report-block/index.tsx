import React, { useCallback } from 'react';
import { Button } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import styles from './index.less';
import {
  IReportBlock, IReportTextBlock, IReportChartBlock, IReportListBlock,
} from '../../store';
import TextBlock from './components/text-block';
import ChartBlock from './components/chart-block';
import ListBlock from './components/list-block';
import { useProjectReportContext } from '../../context';

interface Props {
  data: IReportBlock
}

const ReportBlock: React.FC<Props> = ({ data }) => {
  const { title, type } = data;
  const { store } = useProjectReportContext();
  const renderBlock = useCallback(() => {
    switch (type) {
      case 'text': {
        return <TextBlock data={data as IReportTextBlock} />;
      }
      case 'chart': {
        return <ChartBlock data={data as IReportChartBlock} />;
      }
      case 'list': {
        return <ListBlock data={data as IReportListBlock} />;
      }
      default: {
        return null;
      }
    }
  }, [data, type]);
  const handleDelete = useCallback(() => {
    store.removeBlock(data.id);
  }, []);
  return (
    <div className={styles.report_block}>
      <div className={styles.header}>
        <span className={styles.title}>{title}</span>
        <div className={styles.operation}>
          <Button icon="edit-o" color={'blue' as ButtonColor}>编辑</Button>
          <Button icon="delete" color={'blue' as ButtonColor} onClick={handleDelete}>删除</Button>
        </div>
      </div>
      {renderBlock()}
    </div>
  );
};

export default ReportBlock;
