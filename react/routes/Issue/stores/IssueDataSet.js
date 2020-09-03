/* eslint-disable import/no-anonymous-default-export */
import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import { Button, Icon } from 'choerodon-ui';
import IssueStore from '@/stores/project/issue/IssueStore';
import Modal from '../components/Modal';
import BatchModal from '../components/BatchModal';

let modal;
function Header({ dataSet, close }) {
  return (
    <>
      <div style={{ fontSize: '18px', fontWeight: 500, marginRight: 12 }}>
        {`批量编辑 (已选中${dataSet.selected.length}项)`}
      </div>
      <Button
        icon="close"
        shape="circle"
        style={{ color: 'white', marginRight: -5, marginLeft: 'auto' }}
        onClick={close}
      />
    </>
  );
}
const ObserverHeader = observer(Header);
function handleSelect({ dataSet }) {
  modal = Modal.open({
    key: 'modal',
    header: <ObserverHeader
      dataSet={dataSet}
      modal={modal}
      close={() => {
        modal.close();
        dataSet.unSelectAll();
      }}
    />,
    content: <BatchModal
      dataSet={dataSet}
      modal={modal}
      fields={IssueStore.fields}
      onCancel={() => {
        modal.close();
        dataSet.unSelectAll();
      }}
      onEdit={() => {
        modal.close();
        dataSet.unSelectAll();
        dataSet.query();
      }}
    />,
  });
}
function handleUnSelect({ dataSet }) {
  if (dataSet.selected.length === 0 && modal) {
    modal.close();
  }
}
export default ({
  projectId, organizationId,
}) => ({
  primaryKey: 'issueId',
  autoQuery: false,
  modifiedCheck: false,
  parentField: 'parentId',
  expandField: 'expand',
  idField: 'issueId',
  paging: 'server',
  cacheSelection: true,
  transport: {
    read: ({ params }) => ({
      url: `/agile/v1/projects/${projectId}/issues/include_sub`,
      method: 'post',
      params: {
        ...params,
        organizationId,
      },
      transformRequest: () => {
        const searchDTO = IssueStore.getCustomFieldFilters();
        return JSON.stringify(searchDTO);
      },
    }),
  },
  fields: [
    { name: 'issueId', type: 'string', label: '概要' },
    { name: 'issueTypeId', type: 'object', label: '问题类型' },
    { name: 'issueNum', type: 'string', label: '任务编号' },
    { name: 'priorityId', type: 'string', label: '优先级' },
    { name: 'statusId', type: 'object', label: '状态' },
    { name: 'assigneeId', type: 'string', label: '经办人' },
    { name: 'reporterId', type: 'string', label: '报告人' },
    { name: 'label', type: 'string', label: '标签' },
    { name: 'component', type: 'string', label: '模块' },
    { name: 'storyPoints', type: 'string', label: '故事点' },
    { name: 'version', type: 'string', label: '版本' },
    { name: 'epic', type: 'string', label: '史诗' },
    { name: 'feature', type: 'string', label: '特性' },
    { name: 'lastUpdateDate', type: 'string', label: '最后更新时间' },
    { name: 'creationDate', type: 'string', label: '创建时间' },
    { name: 'estimatedStartTime', type: 'string', label: '预计开始时间' },
    { name: 'estimatedEndTime', type: 'string', label: '预计结束时间' },
    { name: 'issueSprintVOS', type: 'array', label: '冲刺' },
  ],
  queryFields: [
    { name: 'issueTypeId', type: 'array', label: '问题类型' },
    // { name: 'service', type: 'string', label: service },
    // { name: 'description', type: 'string', label: description },
  ],
  events: {
    select: handleSelect,
    selectAll: handleSelect,
    unSelect: handleUnSelect,
    unSelectAll: handleUnSelect,
    load: ({ dataSet }) => {
      // 有筛选，自动展开
      if (IssueStore.isHasFilter) {
        IssueStore.tableRef.current.tableStore.expandAll();
      }
    },
  },
});
