import React from 'react';
import IssueSearch from '@/components/issue-search';
import IssueSearchStore from '@/components/issue-search/store';
import openSaveFilterModal from '@/components/SaveFilterModal';
import { useGanttContext } from '../../context';

interface Props {
  issueSearchStore: IssueSearchStore
  loadData: () => void
}
const GanttIssueSearch: React.FC<Props> = ({ issueSearchStore, loadData }) => {
  const { projectId } = useGanttContext();
  // const issueSearchStore = useIssueSearchStore({
  //   getSystemFields: () => getSystemFields().filter((item) => item.code !== 'sprint') as ILocalField[],
  //   transformFilter,
  // });
  const handleSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList, projectId });
  };

  return (
    <IssueSearch
      projectId={projectId}
      store={issueSearchStore}
      onClear={loadData}
      onChange={loadData}
      onClickSaveFilter={handleSaveFilter}
      foldedHeight={42}
    />
  );
};
export default GanttIssueSearch;
