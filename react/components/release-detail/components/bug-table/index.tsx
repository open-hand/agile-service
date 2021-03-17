import React from 'react';
import {
  Button, Modal, Table, Tooltip,
} from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import UserTag from '@/components/tag/user-tag';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import { versionApi } from '@/api';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import renderTags from '@/components/column-renderer/tags';
import { useReleaseDetailContext } from '../../stores';

const { Column } = Table;
function BugTable() {
  const { bugTableDataSet, store } = useReleaseDetailContext();
  function handleDelete(record: Record) {
    Modal.confirm({
      title: '是否删除？',
      children: `确定要删除问题${record.get('issueNum')}与版本的关联？删除后，将移除缺陷和当前版本下应用版本的关联关系。`,
      onOk: () => versionApi.deleteLinkIssueId(record.get('issueId'), store.current?.versionId!).then(() => {
        bugTableDataSet.query();
      }),
    });
  }
  function renderAction({ record }: RenderProps) {
    return <Button icon="delete_forever" onClick={() => handleDelete(record!)} />;
  }
  function renderSummary({ value }: RenderProps) {
    return (
      <Tooltip title={value} placement="topLeft">
        <span className="c7n-agile-table-cell-click">{value}</span>
      </Tooltip>
    );
  }

  return (
    <Table dataSet={bugTableDataSet}>
      <Column
        name="summary"
        onCell={({ record }) => ({
          onClick: () => {
            store.selectIssue(record.get('issueId'));
          },
        })}
        lock={'left' as any}
        width={210}
        renderer={renderSummary}
      />
      <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
      <Column name="status" renderer={({ record }) => (record?.get('statusVO') ? renderStatus({ record }) : undefined)} />
      <Column name="priority" renderer={renderPriority} />
      <Column
        name="influenceVersion"
        renderer={({ record }) => {
          const influenceArr = record?.get('versionIssueRelVOS')?.filter((i: any) => i.relationType === 'influence') || [];
          return influenceArr.length > 0 ? (
            <Tooltip title={<div>{influenceArr.map((item:{name:string}) => <div>{item.name}</div>)}</div>}>
              {renderTags({ array: influenceArr, name: influenceArr[0].name })}
            </Tooltip>
          ) : undefined;
        }}
      />
      <Column
        name="assigneeId"
        className="c7n-agile-table-cell"
        renderer={({ value, record }) => (value ? (
          <UserTag data={{
            imageUrl: record?.get('assigneeImageUrl'),
            loginName: record?.get('assigneeLoginName'),
            realName: record?.get('assigneeRealName'),
            tooltip: record?.get('assigneeName'),
          }}
          />
        ) : '')}
      />
      <Column name="action" lock={'right' as any} renderer={renderAction} width={60} />

    </Table>
  );
}
export default BugTable;
