import React, {
  useCallback, useEffect, useRef, useState,
} from 'react';
import {
  Menu, Tooltip, TextField, Icon, Spin, Dropdown, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import ScrollContext from 'react-infinite-scroll-component';
import { debounce, isEmpty, isEqual } from 'lodash';
import classnames from 'classnames';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { useSize } from 'ahooks';
import { usePublishVersionContext } from '../../stores';
import styles from './index.less';
import { openEditPublishVersionModal } from '../create-edit-publish-version';

interface VersionItemProps {
  name: string
  activeId: number | undefined
  onClick: Function
  data: Record
}
const VersionItem = observer<VersionItemProps>(({
  name, activeId, onClick, data,
}) => {
  const { store, leftListItemConfig } = usePublishVersionContext();
  function handleUpdate(updateData: any, record: any, statusCode?: any) {
    store.update(updateData, record, statusCode);
  }
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
            store.delete(record.get('id'));
          },
        });
        break;
      case 'edit':
        openEditPublishVersionModal({ editData: record.toData(), handleOk: (updateData: any) => handleUpdate(updateData, record) });
        break;
      case 'version_planning':
      case 'released': {
        handleUpdate({ ...record.toData(), statusCode: key }, record, key);

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
        <span role="none" onClick={(e) => e.stopPropagation()}>
          <Dropdown
            overlay={(leftListItemConfig ? leftListItemConfig.renderMenus(data)
              : (
                <Menu onClick={({ key }) => handleClickMenu(key, data!)}>
                  {data?.get('statusCode') === 'version_planning' ? <Menu.Item key="released">发布</Menu.Item>
                    : <Menu.Item key="version_planning">撤销发布</Menu.Item>}
                  <Menu.Item key="edit">编辑</Menu.Item>
                  <Menu.Item key="del">删除</Menu.Item>
                </Menu>
              )
            )}
            trigger={['click'] as any}
          >

            <Icon
              // @ts-ignore
              shape="circle"
              type="more_vert"
            />
          </Dropdown>
        </span>
      </div>
    </Tooltip>
  );
});
function PublishVersionList() {
  const {
    prefixCls, tableDataSet, store, leftListItemConfig,
  } = usePublishVersionContext();
  const [searchText, setSearchText] = useState<string>();
  const scrollRef = useRef(null);
  const scrollSize = useSize(scrollRef);

  function handleChange(data: Record) {
    // data.dataSet?.select(data);
    tableDataSet.select(data);
    store.select(data.toData());
    leftListItemConfig && leftListItemConfig.onChange && leftListItemConfig.onChange(data);
  }
  function handleLoadMore() {
    console.log('load more...');
    tableDataSet.queryMore(tableDataSet.currentPage + 1);
  }

  /** 初始化滚动加载数据 */
  const handleInitResizeData = useCallback(() => {
    const scrollHeight = (scrollSize.height || 0) - 43 - tableDataSet.length * 38;

    if (scrollHeight > 0) {
      // console.log('scrollHeight', scrollSize, scrollHeight);
      const needMoreDataLength = Math.ceil(scrollHeight / 38);
      const needLoadToPageIndex = tableDataSet.currentPage + Math.ceil(needMoreDataLength / tableDataSet.pageSize);
      for (let pageIndex = tableDataSet.currentPage + 1; pageIndex <= tableDataSet.totalPage && pageIndex <= needLoadToPageIndex; pageIndex += 1) {
        tableDataSet.queryMore(pageIndex);
      }
    }
  }, [scrollSize, tableDataSet]);
  // 注册发布版本更新 创建事件
  useEffect(() => {
    function registerCreateAfter() {
      tableDataSet.query().then(() => {
        if (tableDataSet.length > 0) {
          tableDataSet.select(tableDataSet.records[0]);
          store.select(tableDataSet.records[0].toData());
          handleInitResizeData();
        } else { // 无数据时清空详情选择 及版本列表选择
          tableDataSet.unSelectAll();
          store.clear();
        }
      });
    }
    function registerUpdate(newData: any, record: Record) {
      record.set(newData);
    }

    store.init({ update: registerUpdate, createAfter: registerCreateAfter, delete: registerCreateAfter });
  }, [handleInitResizeData, store, tableDataSet]);
  const handleChangeSearch = (val: string) => {
    setSearchText((oldText) => {
      tableDataSet.setState('searchMode', isEmpty(val?.trim()) ? undefined : true);
      tableDataSet.setQueryParameter('content', val?.trim());
      console.log('query....');
      oldText !== val?.trim() && tableDataSet.query().then((res) => {
        handleInitResizeData();
      });
      return isEqual(oldText, val?.trim()) ? oldText : val?.trim();
    });
  };
  const handleFilterChange = debounce((val) => {
    handleChangeSearch(val);
  }, 500);
  useEffect(() => {
    handleInitResizeData();
  }, [handleInitResizeData]);
  useEffect(() => {
    const selectedRecord = tableDataSet.status === 'ready' && tableDataSet.find((record) => record.get('id') === store.getCurrentData.id);
    console.log('selectedRecord...', selectedRecord);
    selectedRecord && tableDataSet.select(selectedRecord);
  }, [store.getCurrentData.id, tableDataSet, tableDataSet.length, tableDataSet.status]);
  return (
    <div className={styles.list} ref={scrollRef}>
      <TextField
        className={styles.search}
        prefix={<Icon type="search" />}
        onChange={handleChangeSearch}
        onInput={(e: any) => handleFilterChange(e.target.value)}
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
          searchText && tableDataSet.length === 0 ? <span className={styles.scroll_bottom}>暂无匹配内容</span>
            : <span className={styles.scroll_bottom}>{tableDataSet.totalPage !== 1 ? '到底了' : ''}</span>

        )}
      >
        {tableDataSet.map((record) => (
          <VersionItem
            data={record}
            name={leftListItemConfig ? leftListItemConfig.renderName(record) : record.get('versionAlias')}
            onClick={handleChange}
            activeId={tableDataSet.currentIndex}
          />
        ))}
      </ScrollContext>
    </div>
  );
}
export default observer(PublishVersionList);
