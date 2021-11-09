import React from 'react';
import IssueSearch from '@/components/issue-search';
import openSaveFilterModal from '@/components/SaveFilterModal';
import { useIssueStore } from '../../stores';

interface Props {
  loadData: () => void
}
const WorkingHoursIssueSearch: React.FC<Props> = ({ loadData }) => {
  const { issueSearchStore } = useIssueStore();
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
export default WorkingHoursIssueSearch;
