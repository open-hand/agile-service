import React, { useContext } from 'react';
import { Button } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { stores, Choerodon } from '@choerodon/boot';
import IssueStore from '@/stores/project/issue/IssueStore';
import { personalFilterApi } from '@/api';
import IssueSearchStore from '@/components/issue-search/store';
import Store from '../../stores';
import FilterItem from './FilterItem';
import './FilterManage.less';

const { HeaderStore } = stores;
interface Props {

}
const FilterManage: React.FC<Props> = () => {
  const { issueSearchStore } = useContext(Store);
  const { myFilters } = issueSearchStore as IssueSearchStore;
  const { filterListVisible } = IssueStore;
  if (!filterListVisible) {
    return null;
  }
  return (
    <div
      className="c7n-filterList"
      style={{ width: 350, top: HeaderStore.announcementClosed ? 50 : 100 }}
    >
      <div className="c7n-filterList-header">
        <span>筛选管理</span>
        <Button
          shape="circle"
          icon="close"
          onClick={() => {
            IssueStore.setFilterListVisible(false);
          }}
        />
      </div>
      {
        myFilters && myFilters.length > 0 ? (
          <ul className="c7n-filterList-content">
            {
              myFilters.map((filter) => (
                <FilterItem
                  data={filter}
                  onSubmit={() => {
                    issueSearchStore.loadMyFilterList();
                  }}
                  onDelete={async () => {
                    await issueSearchStore.loadMyFilterList();
                    if (issueSearchStore.myFilters.length === 0) {
                      IssueStore.setFilterListVisible(false);
                    }
                  }}
                />
              ))
            }
          </ul>
        ) : <div style={{ textAlign: 'center', marginTop: 15, color: 'rgba(0,0,0,0.65)' }}>暂无数据</div>
      }
    </div>
  );
};
export default observer(FilterManage);
