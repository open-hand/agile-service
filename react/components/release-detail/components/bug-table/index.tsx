import React from 'react';
import { Button, Table, Tooltip } from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import UserTag from '@/components/tag/user-tag';
import { useReleaseDetailContext } from '../../stores';

const { Column } = Table;
function BugTable() {
  const { bugTableDataSet, isInProgram } = useReleaseDetailContext();
  function renderAction({}:RenderProps) {
    return <Button icon="delete_forever" />;
  }
  function renderSummary({ value }:RenderProps) {
    return (
      <Tooltip title={value} placement="topLeft">
        <span className="c7n-agile-table-cell-click">{value}</span>
      </Tooltip>
    );
  }
  function renderEpicOrFeature({ record, name: fieldName }:RenderProps) {
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
    <Table dataSet={bugTableDataSet}>
      <Column name="summary" lock={'left' as any} width={210} renderer={renderSummary} />
      <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
      <Column name="status" />
      <Column name="priority" />
      <Column name="influenceVersion" />
      <Column name="assigner" className="c7n-agile-table-cell" renderer={({ value }) => (value ? <UserTag data={value} /> : '')} />
      <Column name="action" lock={'right' as any} renderer={renderAction} width={60} />

    </Table>
  );
}
export default BugTable;
