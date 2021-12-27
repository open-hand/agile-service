import React from 'react';
import {
  get, find, intersection, noop,
} from 'lodash';
import { CheckBox, Tooltip } from 'choerodon-ui/pro';
import { systemColumnsMap, getCustomColumn } from './baseColumns';
import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';
import { ListLayoutColumnVO } from '@/api';
import { IFoundationHeader } from '@/common/types';

export const checkBoxColumn = ({
  checkValues, data, handleCheckChange, handleCheckAllChange, checkValuesRef,
}: { checkValues: string[], data: any[], handleCheckChange: (value: any, issueId: string) => void, handleCheckAllChange: any, checkValuesRef: any }) => {
  const keys = data.map((i) => i.issueId);
  const pageCheckedKeys = intersection(keys, checkValues);
  const checked = pageCheckedKeys.length > 0;
  const allChecked = pageCheckedKeys.length === keys.length;
  const indeterminate = checked && !allChecked;
  return ({
    title: (
      <CheckBox
        indeterminate={indeterminate}
        checked={checked}
        onChange={handleCheckAllChange}
      />
    ),
    dataIndex: 'issueId',
    key: 'issueId',
    width: 40,
    fixed: true,
    render: ({ rowData, dataIndex, rowIndex }: any) => (
      <CheckBox
        key={rowIndex}
        value={rowData.issueId}
        checked={checkValuesRef.current.includes(rowData.issueId)}
        onChange={(value) => handleCheckChange(value, rowData.issueId)}
      />
    ),
  });
};
export const expandColumn = {
  title: '',
  dataIndex: 'issueId',
  key: 'issueId',
  className: 'expand-column',
  width: 10,
  fixed: true,
  treeCol: true,
  render: () => null,
};

const getColumnsMap = ({ onSummaryClick = noop }: { onSummaryClick?: (data: any) => void }) => {
  const summaryColumn = systemColumnsMap.get('summary')! || {} as any;
  const columnMap = new Map<string, any>([['summary', { ...summaryColumn, render: ({ rowData }: any) => summaryColumn?.render && summaryColumn?.render(rowData, get, { onClick: () => onSummaryClick(rowData) }) }]]);
  systemColumnsMap.forEach((column, key) => {
    if (!columnMap.has(key)) {
      columnMap.set(key, { ...column, render: column.render ? ({ rowData }: any) => column.render!(rowData) : undefined });
    }
  });
  return columnMap;
};
export const IntlField: React.FC<{ column: any }> = ({ children, column }) => {
  const formatMessage = useFormatMessage();
  const name = column.titleKey ? formatMessage({ id: column.titleKey }) : column.title; // , defaultMessage: column.title
  return React.isValidElement(name) ? name : <Tooltip title={name}>{name}</Tooltip>;
};
interface IIssueTableColumnsProps {
  listLayoutColumns?: ListLayoutColumnVO[] | null,
  fields: IFoundationHeader[],
  onSummaryClick?: (data: any) => void, handleColumnResize?: (...args: any) => void
  // getColumnsMap?:(...args:any[])=>
}
export function getTableColumns({
  listLayoutColumns, fields, onSummaryClick, handleColumnResize,
}: IIssueTableColumnsProps) {
  const res: any[] = [];
  const columnsMap = getColumnsMap({ onSummaryClick });
  const getCustom = (field: IFoundationHeader) => {
    const column = getCustomColumn(field)!;
    return { ...column, render: ({ rowData }: any) => column.render!(rowData) };
  };
  listLayoutColumns?.forEach((layoutColumn) => {
    const { columnCode: code, width } = layoutColumn;
    const field = find(fields, { code });
    if (field) {
      // 系统字段和自定义字段处理
      const column = columnsMap.has(code) ? columnsMap.get(code) : getCustom(field);
      res.push({
        ...column,
        title: <IntlField column={column} />,
        code,
        display: layoutColumn.display,
        resizable: true,
        onResize: handleColumnResize,
        width: width && width > 0 ? width : column.width,
      });
    }
  });
  return res;
}
