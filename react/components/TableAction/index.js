import React from 'react';
import { Dropdown, Menu, Button } from 'choerodon-ui';
import './index.less';

const prefix = 'c7n-table-action';
const TableAction = (props) => {
  const {
    menus, text, onMenuClick, onEditClick,
  } = props;
  const renderMenu = () => (
    <Menu onClick={onMenuClick}>
      {menus.map((menu) => (
        <Menu.Item key={menu.key}>
          {menu.text}
        </Menu.Item>
      ))}
    </Menu>
  );
  const clickable = !!onEditClick;
  return (
    <div className={prefix}>
      <span style={{ display: 'flex', overflow: 'hidden' }}>
        {clickable ? (
          <a
            className="c7n-agile-table-cell-click"
            style={{ overflow: 'hidden' }}
            role="none"
            onClick={onEditClick}
            onKeyDown={null}
          >
            {text}
          </a>
        ) : <span className="c7n-agile-table-cell">{text}</span>}
      </span>
      {
        menus.length > 0 ? (
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <Dropdown overlay={renderMenu()} trigger="click">
              <Button shape="circle" icon="more_vert" style={{ color: 'var(--primary-color)' }} />
            </Dropdown>
          </div>
        ) : null
      }
    </div>
  );
};

export default TableAction;
