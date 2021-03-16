import React from 'react';
import {
  Button, Modal, Table, Tooltip,
} from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import UserTag from '@/components/tag/user-tag';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { versionApi } from '@/api';
import { useReleaseDetailContext } from '../../stores';

const { Column } = Table;
function StoryTable() {
  const { storyTableDataSet, isInProgram } = useReleaseDetailContext();

  function handleDelete(record: Record) {
    Modal.confirm({
      title: '是否删除关联',
      children: `您确定要删除问题${record.get('issueNum')}与版本的关联？`,
      onOk: () => versionApi.deleteLinkIssueId(record.get('issueId'), '').then((res:any) => {
        storyTableDataSet.query();
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
  function renderEpicOrFeature({ record, name: fieldName }: RenderProps) {
    const color = fieldName === 'epic' ? record?.get('epicColor') : record?.get('featureColor');
    const name = fieldName === 'epic' ? record?.get('epicName') : record?.get('featureName');
    return name ? (
      <Tooltip title={name}>
        <span style={{
          width: '100%',
          color,
          borderWidth: '1px',
          borderStyle: 'solid',
          borderColor: color,
          borderRadius: '2px',
          fontSize: '13px',
          lineHeight: '20px',
          padding: '0 4px',
          display: 'inline-block',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          whiteSpace: 'nowrap',
        }}
        >
          {name}
        </span>
      </Tooltip>
    ) : null;
  }

  return (
    <Table dataSet={storyTableDataSet}>
      <Column name="summary" lock={'left' as any} width={210} renderer={renderSummary} />
      <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
      <Column name="status" />
      {/* 应用服务是否存在的判断 */}
      <Column name="appService" className="c7n-agile-table-cell" />
      <Column name={isInProgram ? 'featureVO' : 'epic'} renderer={renderEpicOrFeature} className="c7n-agile-table-cell" />
      <Column name="assigner" className="c7n-agile-table-cell" renderer={({ value }) => (value ? <UserTag data={value} /> : '')} />
      <Column name="action" lock={'right' as any} renderer={renderAction} width={60} />

    </Table>
  );
}
export default StoryTable;
