import React from 'react';
import { Button } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { stores } from '@choerodon/boot';
import IssueSearchStore from '@/components/issue-search/store';
import useGetAnnouncementHeight from '@/hooks/useGetAnnouncementHeight';
import FilterItem from './FilterItem';
import './FilterManage.less';

const { HeaderStore } = stores;
interface Props {
  visible: boolean,
  setVisible: Function,
  /** @default project */
  menuType?: 'project' | 'org',
  issueSearchStore: IssueSearchStore,
}
const FilterManage: React.FC<Props> = ({
  visible, menuType = 'project', setVisible, issueSearchStore,
}) => {
  const { myFilters } = issueSearchStore;
  const announcementHeight = useGetAnnouncementHeight();
  if (!visible) {
    return null;
  }
  return (
    <div
      className="c7n-filterList"
      style={{ width: 350, top: `calc(50px + ${announcementHeight}` }}
    >
      <div className="c7n-filterList-header">
        <span>个人筛选</span>
        <Button
          shape="circle"
          icon="close"
          onClick={() => {
            setVisible(false);
          }}
        />
      </div>
      {
        myFilters && myFilters.length > 0 ? (
          <ul className="c7n-filterList-content">
            {
              myFilters.map((filter) => (
                <FilterItem
                  key={filter.filterId}
                  menuType={menuType}
                  data={filter}
                  onSubmit={async () => {
                    await issueSearchStore.loadMyFilterList();
                  }}
                  onDelete={async () => {
                    await issueSearchStore.loadMyFilterList();
                    if (issueSearchStore.myFilters.length === 0) {
                      setVisible(false);
                    }
                  }}
                />
              ))
            }
          </ul>
        ) : <div style={{ textAlign: 'center', marginTop: 15, color: 'var(--text-color3)' }}>暂无数据</div>
      }
    </div>
  );
};
export default observer(FilterManage);
