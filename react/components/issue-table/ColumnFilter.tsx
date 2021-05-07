import React, { useMemo, useCallback, useState } from 'react';
import { useControllableValue } from 'ahooks';
import {
  Menu, Dropdown, Button,
} from 'choerodon-ui/pro';
import { Placements } from 'choerodon-ui/pro/lib/dropdown/interface';

const prefixCls = 'c7n-pro-table-columns-chooser-dropdown-menu';

export interface ColumnFilterProps {
  options: { code: string, title: string }[]
  value?: string[]
  onChange?: (value: string[]) => void
}
const ColumnFilter: React.FC<ColumnFilterProps> = (props) => {
  const { options } = props;
  const [hidden, setHidden] = useState(true);
  const handleHiddenChange = useCallback((v: boolean) => {
    setHidden(v);
  }, []);
  const [selectedKeys, setSelectedKeys] = useControllableValue<string[]>(props, {
    defaultValue: [],
  });
  const isSelected = useCallback((key: string) => selectedKeys.includes(key), [selectedKeys]);
  const choose = useCallback((key) => {
    setSelectedKeys([...selectedKeys, key]);
  }, [selectedKeys, setSelectedKeys]);
  const unChoose = useCallback((key) => {
    setSelectedKeys(selectedKeys.filter((k) => k !== key));
  }, [selectedKeys, setSelectedKeys]);
  const handleMenuClick = useCallback(({
    key,
    item: {
      props: { value },
    },
  }) => {
    if (isSelected(value)) {
      unChoose(value);
    } else {
      choose(value);
    }
  }, [choose, isSelected, unChoose]);
  const menu = useMemo(() => (
    <Menu
      prefixCls={prefixCls}
      focusable={false}
      multiple
      selectedKeys={selectedKeys}
      onClick={handleMenuClick}
    >
      {options.map((option) => (
        <Menu.Item key={option.code} value={option.code}>
          {option.title}
        </Menu.Item>
      ))}
    </Menu>
  ), [handleMenuClick, options, selectedKeys]);
  return (
    <div>
      <Dropdown
        overlay={menu}
        placement={'bottomRight' as Placements}
        hidden={hidden}
        onHiddenChange={handleHiddenChange}
      >
        <Button icon="view_column" size={'small' as any} />
      </Dropdown>
    </div>
  );
};

export default ColumnFilter;
