import React, { useCallback } from 'react';
import { Dropdown, Menu } from 'choerodon-ui';
import { Icon } from 'choerodon-ui/pro';
import './index.less';

const prefix = 'c7n-table-action';
const TableAction = (props) => {
  const {
    menus, text, onMenuClick, onEditClick,
  } = props;
  const clickable = !!onEditClick;
  const handleClickMenu = useCallback((params) => {
    if (params.key === 'menuEdit' && clickable) {
      onEditClick();
      return;
    }
    onMenuClick && onMenuClick(params);
  }, [clickable, onEditClick, onMenuClick]);
  const renderMenu = () => (
    <Menu onClick={handleClickMenu}>
      {clickable ? <Menu.Item key="menuEdit">修改</Menu.Item> : null}
      {menus.map((menu) => (
        <Menu.Item key={menu.key}>
          {menu.text}
        </Menu.Item>
      ))}
    </Menu>
  );
  return (
    <div className={prefix}>
      <span style={{ display: 'flex', overflow: 'hidden' }}>
        <span className="c7n-agile-table-cell">{text}</span>
      </span>
      {
        menus.length > 0 ? (
          <div style={{ display: 'flex', alignItems: 'center', cursor: 'pointer' }}>
            <Dropdown overlay={renderMenu()} trigger="click">
              <Icon type="more_vert" style={{ color: 'var(--primary-color)' }} />
            </Dropdown>
          </div>
        ) : null
      }
    </div>
  );
};

export default TableAction;
