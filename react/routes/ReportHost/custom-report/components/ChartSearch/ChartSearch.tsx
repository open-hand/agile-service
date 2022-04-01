import React from 'react';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { ISearchVO } from '@/common/types';

import styles from './ChartSearch.less';
import useIsWaterfall from '@/hooks/useIsWaterfall';

export interface CustomReportSearchProps {
  projectId?: string
  searchVO?: ISearchVO
  setSearchVO: (searchVO: ISearchVO) => void
  searchCls?: string
}

const ChartSearch: React.FC<CustomReportSearchProps> = ({
  searchVO, setSearchVO, projectId, searchCls,
}) => {
  const { isWaterfall } = useIsWaterfall();
  const issueSearchStore = useIssueSearchStore({
    projectId,
    transformFilter,
    defaultSearchVO: searchVO,
    // @ts-ignore
    getSystemFields: () => getSystemFields().filter((f) => !['contents'].includes(f.code)),
  });
  return (
    <div className={classNames(styles.chartSearch, searchCls)}>
      <IssueSearch
        projectId={projectId}
        applyType={isWaterfall ? '' : 'agile'}
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
