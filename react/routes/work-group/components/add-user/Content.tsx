import React, { useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Button, CheckBox, Icon, TextField, Spin,
} from 'choerodon-ui/pro';
import { message } from 'choerodon-ui';
import { map } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { useAddUserStore } from '@/routes/work-group/components/add-user/stores';
import Styles from './index.less';
import { workGroupApi } from '@/api/WorkGroup';

const AddUserContent = () => {
  const {
    tableDs,
    modal,
    workGroupId,
    refresh,
  } = useAddUserStore();

  modal.handleOk(async () => {
    try {
      const userIds = map(tableDs.selected, (record: Record) => record.get('id'));
      if (!userIds?.length) {
        message.info('请至少选择一名成员');
        return false;
      }
      await workGroupApi.addUserByGroup(workGroupId, userIds);
      refresh && refresh();
      return true;
    } catch (e) {
      return false;
    }
  });

  const indeterminate = useMemo(() => {
    const selectedLength = tableDs.selected.length;
    return !!(selectedLength && selectedLength !== tableDs.length);
  }, [tableDs.selected, tableDs.length]);

  const handleSearch = useCallback((value) => {
    tableDs.setQueryParameter('param', value);
    tableDs.query();
  }, []);

  const handleLoadMore = useCallback(() => {
    const page = tableDs.currentPage + 1;
    tableDs.queryMore(page);
  }, [tableDs.currentPage]);

  const handleSelect = useCallback((checked, record: Record) => {
    if (checked) {
      tableDs.select(record);
    } else {
      tableDs.unSelect(record);
    }
  }, []);

  const handleSelectAll = useCallback((checked) => {
    if (checked) {
      tableDs.selectAll();
    } else {
      tableDs.unSelectAll();
    }
  }, []);

  return (
    <div className={Styles.addUserWrap}>
      <TextField
        placeholder="搜索成员"
        onChange={handleSearch}
        prefix={<Icon type="search" />}
        className={Styles.searchInput}
      />
      <div>
        <CheckBox
          checked={!!(!indeterminate && tableDs.selected?.length)}
          indeterminate={indeterminate}
          onChange={handleSelectAll}
          className={Styles.checkAll}
        >
          全选
        </CheckBox>
      </div>
      <Spin spinning={tableDs.status === 'loading'}>
        <div className={Styles.list}>
          {tableDs.map((record: Record) => (
            <div key={record.id} className={Styles.item}>
              <CheckBox
                checked={record.isSelected}
                onChange={(checked) => handleSelect(checked, record)}
              />
              <span className={Styles.realName}>{record.get('realName')}</span>
              <span>{record.get('loginName')}</span>
            </div>
          ))}
        </div>
      </Spin>
      {tableDs.totalPage > tableDs.currentPage && (
        <Button
          onClick={handleLoadMore}
          funcType={FuncType.flat}
        >
          加载更多
        </Button>
      )}
    </div>
  );
};

export default observer(AddUserContent);
