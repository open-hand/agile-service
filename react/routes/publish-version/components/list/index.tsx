import React, {
  memo, useEffect, useRef, useState,
} from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, TextField, Icon, Spin, Dropdown, Modal,
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
import { useSize } from 'ahooks';
import { usePublishVersionContext } from '../../stores';

import styles from './index.less';
import { openEditPublishVersionModal } from '../create-edit-publish-version';

interface VersionItemProps {
  name: string
  activeId: number | undefined
  onClick: Function
  onRefresh: Function
  data: Record
}
const VersionItem = observer<VersionItemProps>(({
  name, activeId, onClick, data, onRefresh,
}) => {
  function handleClickMenu(key: string, record: Record) {
    switch (key) {
      case 'del':
        Modal.confirm({
          title: '删除发布版本',
          children: (
            <div>
              <span>{`您确定要删除发布版本【${record.get('versionAlias') || record.get('version')}】？`}</span>
            </div>),
          onOk: () => {
            publishVersionApi.delete(record.get('id')).then(() => {
              onRefresh();
            });
          },
        });
        break;
      case 'edit':
        openEditPublishVersionModal({ editData: record.toData() });
        break;
      case 'version_planning':
      case 'released': {
        publishVersionApi.update(record.get('id'), {
          ...record.toData(),
          statusCode: key,
        }, key).then(() => onRefresh());
        break;
      }
      default:
        break;
    }
  }
  return (
    <Tooltip title={name} placement="top">
      <div
        role="none"
        className={classnames(styles.version_item,
          { [styles.version_item_active]: data.isSelected })}
        onClick={() => onClick(data)}
      >
        <span className={styles.version_item_text}>{name}</span>
        <Dropdown
          overlay={(
            <Menu onClick={({ key }) => handleClickMenu(key, data!)}>
              {data?.get('statusCode') === 'version_planning' ? <Menu.Item key="released">发布</Menu.Item>
                : <Menu.Item key="version_planning">撤销发布</Menu.Item>}
              <Menu.Item key="edit">编辑</Menu.Item>
              <Menu.Item key="del">删除</Menu.Item>
            </Menu>
          )}
          trigger={['click'] as any}
        >
          <Icon
            // @ts-ignore
            shape="circle"
            type="more_vert"
          />
        </Dropdown>
      </div>
    </Tooltip>
  );
});
function PublishVersionList() {
  const { prefixCls, tableDataSet, store } = usePublishVersionContext();
  const scrollRef = useRef(null);
  const scrollSize = useSize(scrollRef);
  const handleFilterChange = debounce((val) => {
    // pIAimProjectStore.setSearchVal(val);
  }, 300);
  function handleChange(data: Record) {
    // data.dataSet?.select(data);
    tableDataSet.select(data);
    store.select(data.toData());
    console.log('data...1', data, data.index, data.isSelected);
  }
  function handleLoadMore() {
    console.log('load more...');
    tableDataSet.queryMore(tableDataSet.currentPage + 1);
  }
  function handleRefresh() {

  }
  useEffect(() => { console.log('selected', tableDataSet.selected, tableDataSet.selected.length); }, [tableDataSet.selected, tableDataSet.selected.length]);
  useEffect(() => {
    const scrollHeight = (scrollSize.height || 0) - 43 - tableDataSet.length * 38;
    console.log('scrollHeight', scrollSize, scrollHeight);

    if (scrollHeight > 0) {
      const needMoreDataLength = Math.ceil(scrollHeight / 38);
      const needLoadToPageIndex = tableDataSet.currentPage + Math.ceil(needMoreDataLength / tableDataSet.pageSize);
      for (let pageIndex = tableDataSet.currentPage + 1; pageIndex <= tableDataSet.totalPage && pageIndex <= needLoadToPageIndex; pageIndex += 1) {
        tableDataSet.queryMore(pageIndex);
      }
    }
  }, [scrollSize, tableDataSet]);
  return (
    <div className={styles.list} ref={scrollRef}>
      <TextField
        className={styles.search}
        prefix={<Icon type="search" />}
        onChange={handleFilterChange}
        placeholder="请输入搜索条件"
        clearButton
      />
      <ScrollContext
        className={styles.scroll}
        dataLength={tableDataSet.length}
        next={handleLoadMore}
        hasMore={tableDataSet.currentPage < tableDataSet.totalPage}
        loader={<Spin spinning className={`${prefixCls}-left-project-scroll-spin`} />}
        height="100%"
        endMessage={(
          <span className={styles.scroll_bottom}>{tableDataSet.totalPage !== 1 ? '到底了' : ''}</span>
        )}
      >
        {tableDataSet.map((record) => <VersionItem data={record} name={record.get('versionAlias')} onClick={handleChange} onRefresh={handleRefresh} activeId={tableDataSet.currentIndex} />)}
      </ScrollContext>
    </div>
  );
}
export default observer(PublishVersionList);
