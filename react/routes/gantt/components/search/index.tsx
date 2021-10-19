import React from 'react';
import IssueSearch from '@/components/issue-search';
import IssueSearchStore from '@/components/issue-search/store';
import openSaveFilterModal from '@/components/SaveFilterModal';

interface Props {
  issueSearchStore: IssueSearchStore
  loadData: () => void
}
const GanttIssueSearch: React.FC<Props> = ({ issueSearchStore, loadData }) => {
  // const issueSearchStore = useIssueSearchStore({
  //   getSystemFields: () => getSystemFields().filter((item) => item.code !== 'sprint') as ILocalField[],
  //   transformFilter,
  // });
  const handleSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };
  return (
    <IssueSearch
      store={issueSearchStore}
      onClear={loadData}
      onChange={loadData}
      onClickSaveFilter={handleSaveFilter}
      foldedHeight={42}
    />
  );
};
export default GanttIssueSearch;
