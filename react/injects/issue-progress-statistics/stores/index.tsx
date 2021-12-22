import React, {
  createContext, useCallback, useContext,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  cloneDeep,
  find, findIndex, includes, set, sortBy,
} from 'lodash';
import { useIssueSearchStore } from '@/components/issue-search';
import type { IssueSearchStore } from '@/components/issue-search';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import useTable from '@/hooks/useTable';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { IFoundationHeader } from '@/common/types';
import { ListLayoutColumnVO } from '@/api';
import { IssueStatisticExtraFields } from '../components/issue-table/utils';

interface IIssueProgressStatisticsContext {
  issueSearchStore: IssueSearchStore
  tableProps: ReturnType<typeof useTable>
  tableFields: IFoundationHeader[]
  listLayoutColumns: ListLayoutColumnVO[]
}
const Store = createContext({} as IIssueProgressStatisticsContext);

export function useIssueProgressStatisticsContext() {
  return useContext(Store);
}
const defaultVisibleColumns = [
  'summary',
  'epic',
  'sprint',
  'status',
  'storyPoints',
  'issueStatistic',
  'workTimeStatistic',
  'storyPointStatistic',
  'environmentBugStatistic',
  'nonEnvironmentBugStatistic',
];

const defaultListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));
function getIssueProgressStatisticsSystemFields() {
  const fields = getSystemFields();
  const sortFieldValue = {
    pi: 10, sprint: 15, feature: 35, epic: 36, statusId: 40,
  };
  fields.push({
    code: 'pi', name: 'PI', noDisplay: false, defaultShow: true,
  });
  const newNameWithKey = {
    epic: { name: '所属史诗', nameKey: undefined },
    feature: { name: '所属特性', nameKey: undefined },
  };
  const featureOrEpicField = cloneDeep(find(fields, { code: 'feature' }) || find(fields, { code: 'epic' }));
  const displayCodes = Object.keys(sortFieldValue).concat(['contents', 'quickFilterIds']);
  if (featureOrEpicField) {
    featureOrEpicField.name = newNameWithKey[featureOrEpicField.code as keyof typeof newNameWithKey].name;
    featureOrEpicField.nameKey = newNameWithKey[featureOrEpicField.code as keyof typeof newNameWithKey].nameKey;
    fields.splice(findIndex(fields, { code: featureOrEpicField.code }), 1, featureOrEpicField);
  }
  return sortBy(fields.map((item) => ({ ...item, defaultShow: includes(displayCodes, item.code) })), (field) => sortFieldValue[field.code as keyof typeof sortFieldValue] || 120) as any[];
}
export const StoreProvider = observer((props) => {
  const {
    children,
  } = props;
  const { data: tableFields = [] } = useIssueTableFields({ extraFields: IssueStatisticExtraFields });

  const issueSearchStore = useIssueSearchStore({ getSystemFields: getIssueProgressStatisticsSystemFields, transformFilter });
  const getTableData = useCallback(({
    page, sort, size, isTree,
  }) => {
    const search = issueSearchStore.getCustomFieldFilters();
    // set(search, 'searchArgs.tree', isTree);
    return new Promise(() => ({}));
  }, [issueSearchStore]);
  const tableProps = useTable(getTableData, {
    rowKey: 'issueId',
    isTree: false,
    defaultPage: 1,
    defaultPageSize: 10,
    // defaultVisibleColumns: cached?.visibleColumns ?? defaultVisibleColumns,
    autoQuery: false,
    // checkBefore: handleCheckBefore,
  });
  const value = {
    ...props,
    issueSearchStore,
    tableProps,
    tableFields,
    listLayoutColumns: defaultListLayoutColumns as any[],
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
});
