import React from 'react';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import { ILocalField } from '@/components/issue-search/store';
import { transformFilter } from './util';

interface Props {
}
const GanttIssueSearch: React.FC<Props> = () => {
  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields().filter((item) => item.code !== 'sprint') as ILocalField[],
    transformFilter,
  });
  const handleClear = () => {

  };
  const handleChange = () => {
    console.log('issueSearchStore', issueSearchStore.chosenFields);
  };
  const handleSaveFilter = () => {};
  return (
    <div>
      <IssueSearch
        store={issueSearchStore}
        onClear={handleClear}
        onChange={handleChange}
        onClickSaveFilter={handleSaveFilter}
      />
    </div>
  );
};
export default GanttIssueSearch;
