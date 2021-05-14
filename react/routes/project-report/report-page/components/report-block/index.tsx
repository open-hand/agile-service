import React, { useCallback } from 'react';
import classNames from 'classnames';
import { Button, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import {
  IReportBlock, IReportTextBlock, IReportChartBlock, IReportListBlock,
} from '../../store';
import TextBlock from './components/text-block';
import ChartBlock from './components/chart-block';
import ListBlock from './components/list-block';
import { useProjectReportContext } from '../../context';
import openAddModal from '../add-modal';
import styles from './index.less';
import listStyles from '../../index.less';

interface BlockProps {
  data: IReportBlock
  index: number
  provided: any
}
interface BlockPreviewProps {
  data: IReportBlock
  index: number
  preview: true
}
type Props = BlockProps | BlockPreviewProps

const ReportBlock: React.FC<Props> = (props) => {
  const { data, index, provided } = props as BlockProps;
  const isPreview = (props as BlockPreviewProps).preview;
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
      case 'static_list': {
        return <ListBlock data={data as IReportListBlock} />;
      }
      case 'dynamic_list': {
        return <ListBlock data={data as IReportListBlock} />;
      }
      default: {
        return null;
      }
    }
  }, [data, type]);
  const handleDelete = useCallback(() => {
    store.removeBlock(index);
  }, [index, store]);
  const handleEdit = useCallback(() => {
    openAddModal({
      data,
      store,
      index,
    });
  }, [data, index, store]);
  return (
    <div className={`${styles.report_block} c7n-project-report-block`}>
      <div
        className={classNames(styles.header, {
          [styles.header_preview]: isPreview,
        })}
        {...provided ? provided.dragHandleProps : {}}
      >
        {isPreview && <div className={listStyles.tip} />}
        {!isPreview && <Icon type="baseline-drag_indicator" style={{ color: 'rgba(0, 0, 0, 0.54)', marginRight: 8 }} />}
        <span className={styles.title}>{title}</span>
        {!isPreview && (
          <div className={styles.operation}>
            <Button icon="mode_edit" color={'primary' as ButtonColor} onClick={handleEdit}>编辑</Button>
            <Button icon="delete" color={'primary' as ButtonColor} onClick={handleDelete}>删除</Button>
          </div>
        )}
      </div>
      <div className={styles.content}>
        {renderBlock()}
      </div>
    </div>
  );
};

export default observer(ReportBlock);
