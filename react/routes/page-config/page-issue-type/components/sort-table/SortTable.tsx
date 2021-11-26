import React, { ReactElement } from 'react';
import { observer } from 'mobx-react-lite';
import {
  DropResult, ResponderProvided, DragDropContext, DragStart,
} from 'react-beautiful-dnd';
import { Icon, Tooltip } from 'choerodon-ui/pro';
import classnames from 'classnames';
import { useCreation } from 'ahooks';
import { IFiledProps, pageConfigApi } from '@/api';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import DropContent from './DropContent';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Props {
  disabled: boolean | undefined,
  org?: number,
  dataStatus: { code: string },
  onDelete?: (data: IFiledProps) => void,
}
function renderLabelRequire(value: string | ReactElement) {
  return (
    <div>
      {value}
      <Tooltip title="必填只是针对创建页有效" placement="top">
        <Icon type="help " className="c7n-page-issue-detail-header-item-icon" />
      </Tooltip>
    </div>
  );
}
function renderOperate() {
  return <span style={{ position: 'absolute' }}>操作</span>;
}

const SortTable: React.FC = () => {
  const formatMessage = useFormatMessage('agile.page');
  const columns: Array<{ name: string, label: React.ReactNode, type?: string, render?: (value: React.ReactNode) => React.ReactNode }> = useCreation(() => [
    { name: 'fieldName', label: formatMessage({ id: 'field.name' }), type: 'common' },
    { name: 'fieldOrigin', label: formatMessage({ id: 'field.source' }), type: 'project' },
    { name: 'defaultValue', label: formatMessage({ id: 'default' }), type: 'organization' },

    {
      name: 'required', label: formatMessage({ id: 'config.project.require' }), type: 'project', render: renderLabelRequire,
    },
    {
      name: 'required', label: '必填（控制项目）', type: 'organization', render: renderLabelRequire,
    },
    { name: 'edited', label: formatMessage({ id: 'field.can.edit' }), type: 'common' },
    {
      name: 'created',
      label: formatMessage({ id: 'field.can.create' }),
      type: 'common',
      render: (value: any) => (
        <div>
          {value}
          <span style={{ float: 'right', paddingRight: '.3rem' }}>{ formatMessage({ id: 'field.operate' })}</span>
        </div>
      ),
    },

    // {
    //   name: 'operate', label: '操作', type: 'project', render: renderOperate,
    // },

  ], []);

  const { sortTableDataSet, pageIssueTypeStore } = usePageIssueTypeStore();
  const { isProject, prefixCls } = useSortTableContext();
  const type = !isProject ? 'organization' : 'project';
  // @ts-ignore
  const onDragStart = (initial: DragStart, provided: ResponderProvided) => {

  };
  // @ts-ignore
  const onDragEnd = async (result: DropResult, provided: ResponderProvided) => {
    const { destination, source } = result;
    if (!destination) {
      return;
    }
    if (destination.index === source.index) {
      return;
    }

    const sourceRecord = sortTableDataSet.data[source.index];
    const destinationRecord = sortTableDataSet.data[destination.index];
    const rankObj = {
      previousRank: null,
      nextRank: null,
    };
    if (destination.index > source.index) {
      rankObj.nextRank = destinationRecord.get('rank');
      rankObj.previousRank = sortTableDataSet.length - 1 !== destination.index ? sortTableDataSet.data[destination.index + 1].get('rank') : null;
    } else {
      rankObj.nextRank = destination.index !== 0 ? sortTableDataSet.data[destination.index - 1].get('rank') : null;
      rankObj.previousRank = destinationRecord.get('rank');
    }
    // 在异步函数中调用 防止不修改record
    // const outSetRank = (newRank: string) => sourceRecord.set('rank', newRank);
    sortTableDataSet.move(source.index, destination.index);
    await pageConfigApi.loadRankValue(rankObj).then((newRank: string) => {
      sourceRecord.set('rank', newRank);
    });
  };
  return (
    <div className={prefixCls}>
      <div className={classnames(`${prefixCls}-header `, { [`${prefixCls}-header-split`]: true })}>
        {columns.filter((item) => item.type === 'common' || item.type === type).map((itemProps) => <span className={`${prefixCls}-header-item`}>{itemProps.render ? itemProps.render(itemProps.label) : itemProps.label}</span>)}
      </div>
      <div className={`${prefixCls}-content`}>
        <DragDropContext
          onDragEnd={onDragEnd}
          onDragStart={onDragStart}
        >
          <div className={`${prefixCls}-drop-wrap`}>
            <DropContent rows={sortTableDataSet.data} isDropDisabled={!pageIssueTypeStore.currentIssueType.enabled} />
          </div>
        </DragDropContext>
      </div>

    </div>
  );
};
export default observer(SortTable);
