import React, { useState, useEffect, useCallback } from 'react';
import {
  CheckBox, Button, Icon,
} from 'choerodon-ui/pro';
import { Popover } from 'choerodon-ui';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { pull } from 'lodash';
import { quickFilterApi } from '@/api';
import { IQuickFilter } from '@/components/quick-search';

interface Props {
  onChange: (filterIds: string[]) => void
  value: string[]
  projectId?: string
}
const QuickFilter: React.FC<Props> = ({
  onChange, value, projectId,
}) => {
  const [quickFilterList, setQuickFilterList] = useState<IQuickFilter[]>([]);
  const refresh = useCallback(async () => {
    const list = await quickFilterApi.project(projectId).loadAll();
    setQuickFilterList(list);
  }, []);
  useEffect(() => {
    refresh();
  }, [refresh]);

  return (
    <Popover
      placement="bottom"
      trigger="click"
      content={quickFilterList.length > 0 ? quickFilterList.map((quickFilter) => (
        <CheckBox
          key={quickFilter.filterId}
          style={{ display: 'block', margin: '5px 0' }}
          checked={value.includes(quickFilter.filterId)}
          onChange={(checked) => {
            if (checked) {
              onChange(value.concat(quickFilter.filterId));
            } else {
              onChange(pull([...value], quickFilter.filterId));
            }
          }}
        >
          {quickFilter.name}
        </CheckBox>
      )) : (
        <div style={{ color: 'rgba(0,0,0,0.65)' }}>
          暂无数据
        </div>
      )}
    >
      <Button
        color={'primary' as ButtonColor}
        style={{
          marginLeft: 20,
        }}
      >
        快速搜索
        <Icon type="baseline-arrow_drop_down" style={{ marginTop: -3 }} />
      </Button>
    </Popover>
  );
};
export default QuickFilter;
