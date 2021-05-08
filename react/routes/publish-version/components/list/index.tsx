import React, { memo } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, TextField, Icon, Spin,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import ScrollContext from 'react-infinite-scroll-component';
import {
  omit, debounce,
} from 'lodash';
import classnames from 'classnames';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import SideNav from '@/components/side-nav';
import { usePublishVersionContext } from '../../stores';

import styles from './index.less';

interface VersionItemProps {
    name: string
    activeId: string | undefined
    onClick: Function
    data: any
}
const VersionItem = memo<VersionItemProps>(({
  name, activeId, onClick, data,
}) => (
  <Tooltip title={name} placement="top">
    <div
      role="none"
      className={classnames(styles.version_item,
        { [styles.version_item_active]: activeId === data.id })}
      onClick={() => onClick(data)}
    >
      <span className={styles.version_item_text}>{name}</span>
    </div>
  </Tooltip>
));
function PublishVersionList() {
  const { prefixCls, tableDataSet } = usePublishVersionContext();
  const handleFilterChange = debounce((val) => {
    // pIAimProjectStore.setSearchVal(val);
  }, 300);
  function handleChange(data:any) {

  }
  function handleLoadMore() {

  }
  return (
    <div className={styles.list}>
      <TextField
        className={styles.search}
        prefix={<Icon type="search" />}
        onChange={handleFilterChange}
        placeholder="请输入搜索条件"
        clearButton
      />
      <ScrollContext
        className={styles.scroll}
        dataLength={10}
        next={handleLoadMore}
        hasMore={false}
        loader={<Spin spinning className={`${prefixCls}-left-project-scroll-spin`} />}
                // height="100%"
        endMessage={(
          <span style={{ display: 'none' }} className={styles.scroll_bottom}>{false ? '到底了' : ''}</span>
                )}
      >
        <VersionItem data={{} as any} name="222" onClick={handleChange} activeId="999" />
      </ScrollContext>
      <span>2</span>
    </div>
  );
}
export default observer(PublishVersionList);
