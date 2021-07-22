import React from 'react';
import { observer } from 'mobx-react-lite';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { ISearchVO } from '@/common/types';

import styles from './ChartSearch.less';

export interface CustomReportSearchProps {
  projectId?: string
  searchVO?: ISearchVO
  setSearchVO: (searchVO: ISearchVO) => void
}

const ChartSearch: React.FC<CustomReportSearchProps> = ({
  searchVO, setSearchVO, projectId,
}) => {
  const issueSearchStore = useIssueSearchStore({
    projectId,
    transformFilter,
    defaultSearchVO: searchVO,
    // @ts-ignore
    getSystemFields: () => getSystemFields().filter((f) => !['contents'].includes(f.code)),
  });
  return (
    <div className={styles.chartSearch}>
      <IssueSearch
        projectId={projectId}
        applyType="agile"
        store={issueSearchStore}
        onClear={() => {
          const newSearchVO = issueSearchStore.getCustomFieldFilters();
          setSearchVO(newSearchVO);
        }}
        onChange={() => {
          const newSearchVO = issueSearchStore.getCustomFieldFilters();
          setSearchVO(newSearchVO);
        }}
        foldedHeight={40}
      />
    </div>
  );
};

export default observer(ChartSearch);
