import React, { useCallback, useMemo } from 'react';
import {
  Dropdown, Icon, Tooltip, Menu,
} from 'choerodon-ui';
import { Permission } from '@choerodon/boot';
import './TableDropMenu.less';
import { pick } from 'lodash';

/**
 * 表格中的下拉菜单
 * 4个参数
 * menu 菜单 Menu 组件构成的菜单项
 *      无传入则代表无下来菜单，仅渲染text内容
 * text 该列需渲染的文字部分
 * onClickEdit 点击文件编辑事件
 * isHasMenu 布尔类型，有menu传入时默认为true否则默认为false  true代表有下拉菜单
 *            此参数可以用于控制权限时 根据权限是否显示菜单
 * className  不传入则是默认样式
 */

const TableDropMenu = (props) => {
  const {
    menu, isHasMenu = !!menu, text, onClickEdit, className, menuPermissionProps = {}, tooltip, style,
  } = props;
  const { permission } = props;
  const {
    type, projectId, organizationId, service,
  } = permission || props;
  // 渲染文本
  const renderText = () => {
    const finalText = (
      <span>
        {text}
      </span>
    );
    return tooltip ? <Tooltip placement="topLeft" title={typeof (tooltip) !== 'boolean' ? tooltip : text}>{finalText}</Tooltip> : finalText;
  };// cursor: 'pointer'
  const handleClickMenu = useCallback((originOnClick) => (params) => {
    if (params.key === 'menuEdit') {
      onClickEdit && onClickEdit();
      return;
    }
    originOnClick && originOnClick(params);
  }, [onClickEdit]);
  const renderDropdown = useCallback((hasPermission) => {
    let showMenu = hasPermission && isHasMenu && menu ? menu : undefined;
    const editItem = onClickEdit ? <Menu.Item key="menuEdit">修改</Menu.Item> : null;
    if (showMenu && React.isValidElement(showMenu)) {
      const { children: menuItems, onClick } = pick(showMenu.props, ['children', 'onClick']);
      const children = [editItem, ...(menuItems || [])].filter(Boolean);
      const handleClick = handleClickMenu(onClick);
      showMenu = React.cloneElement(showMenu, { ...showMenu.props, children, onClick: handleClick });
    } else if (editItem) {
      showMenu = (
        <Menu onClick={onClickEdit}>
          {editItem}
        </Menu>
      );
    }
    return showMenu ? (
      <div style={{ display: 'flex', alignItems: 'center', cursor: 'pointer' }}>
        <Dropdown overlay={showMenu} trigger="click">
          <Icon shape="circle" type="more_vert" style={{ color: 'var(--primary-color)' }} />
        </Dropdown>
      </div>
    ) : null;
  }, [handleClickMenu, isHasMenu, menu, onClickEdit]);
  return (
    <div
      style={{
        display: 'flex', alignItems: 'center', justifyContent: 'space-between', ...style,
      }}
      className={className || 'table-drop-menu-base'}
    >
      <span style={{
        display: 'inline-block', width: '100%', textOverflow: 'ellipsis', overflow: 'hidden',
      }}
      >
        {permission
          ? (
            <Permission
              type={type}
              projectId={projectId}
              organizationId={organizationId}
              service={service}
            >
              {renderText()}
            </Permission>
          )
          : renderText()}
      </span>
      <Permission {...menuPermissionProps}>
        {renderDropdown}
      </Permission>
    </div>
  );
};

export default TableDropMenu;
