import React from 'react';
import { find, groupBy } from 'lodash';
import { IssueTableProps } from '@/components/issue-table';
import { getTableColumns as defaultGetTableColumns, IntlField } from '@/components/issue-table/columns';
import { IIssueProgressCardType, IssueProgressCardPredefined } from '../issue-progress-card';
import { IFoundationHeader } from '@/common/types';

const statisticChildren = [
  {
    mapPredefinedTypeValueKey: 'total',
    width: 50,
    colSpan: 2,
    resizable: true,
  },
  {
    mapPredefinedTypeValueKey: 'finish',
    width: 66,
    resizable: true,
  },
  {
    mapPredefinedTypeValueKey: 'percentage',
    width: 66,
    resizable: true,
  },
] as const;
const IssueStatisticExtraFields: IFoundationHeader[] = [
  { code: 'issueStatistic', title: '工作项', fieldType: 'number' },
  { code: 'storyPointStatistic', title: '故事点', fieldType: 'number' },
  { code: 'workTimeStatistic', title: '工时', fieldType: 'number' },
  { code: 'environmentBugStatistic', title: '生产环境bug', fieldType: 'number' },
  { code: 'nonEnvironmentBugStatistic', title: '非生产环境bug', fieldType: 'number' },
];
const statisticSystemField: Record<string, {
  children: Array<typeof statisticChildren>[0],
  type: string, align: string, mapPredefinedType: IIssueProgressCardType,
  [key:string]:any
}> = {
  issueStatistic: {
    children: statisticChildren,
    type: 'ColumnGroup',
    align: 'center',
    mapPredefinedType: 'issue',
  },
  storyPointStatistic: {
    children: statisticChildren,
    type: 'ColumnGroup',
    align: 'center',
    mapPredefinedType: 'storyPoint',
  },
  workTimeStatistic: {
    children: statisticChildren,
    type: 'ColumnGroup',
    align: 'center',
    mapPredefinedType: 'workTime',
  },
  environmentBugStatistic: {
    children: statisticChildren,
    type: 'ColumnGroup',
    align: 'center',
    mapPredefinedType: 'environmentBug',
  },
  nonEnvironmentBugStatistic: {
    children: statisticChildren,
    type: 'ColumnGroup',
    align: 'center',
    mapPredefinedType: 'nonEnvironmentBug',
  },
};
const getTableColumns: IssueTableProps['getTableColumns'] = ({
  listLayoutColumns, fields, onSummaryClick, handleColumnResize,
}) => {
  const statisticSystemFieldCodes = Object.keys(statisticSystemField);
  const groupField = groupBy(listLayoutColumns, (item) => (statisticSystemFieldCodes.includes(item.columnCode) ? 'statistic' : 'default'));
  const epicOrFeatureNameMap = {
    epic: { name: '所属史诗', nameKey: undefined },
    feature: { name: '所属特性', nameKey: undefined },
  };
  const res: any[] = defaultGetTableColumns({
    listLayoutColumns: groupField.default, fields, onSummaryClick, handleColumnResize,
  }).map((item) => ({ ...item, title: epicOrFeatureNameMap[item.code as keyof typeof epicOrFeatureNameMap]?.name || item.title, verticalAlign: 'middle' }));
  const statisticSystemFields = groupField.statistic.map((item) => {
    const field = find(fields, { code: item.columnCode });
    if (field) {
      const statisticField = statisticSystemField[field.code as keyof typeof statisticSystemField];
      const typeTemplate = IssueProgressCardPredefined.PredefinedTypeTemplate[statisticField.mapPredefinedType];
      statisticField.children = statisticField.children.map((ch) => ({
        ...ch, title: typeTemplate[ch.mapPredefinedTypeValueKey], key: `${field.code}**${ch.mapPredefinedTypeValueKey}`, dataIndex: `${field.code}**${ch.mapPredefinedTypeValueKey}`,
      } as any)) as any;
      return {
        ...statisticField,
        header: field.title,
        code: field.code,
        display: item.display,
        resizable: true,
      };
    }
    return undefined;
  }).filter(Boolean) as any[];
  res.push(...statisticSystemFields);
  return res;
};
export { getTableColumns, IssueStatisticExtraFields };
