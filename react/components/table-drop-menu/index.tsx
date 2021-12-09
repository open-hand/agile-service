import React, { useCallback, useMemo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import { ButtonProps } from 'choerodon-ui/lib/button';
import { Action, Permission } from '@choerodon/boot';

import './index.less';
import { pick } from 'lodash';
import classNames from 'classnames';
import { IBootActionDataItem, IBootPermissionProps } from '@/common/types';

export interface ITableDropMenuItem extends IBootActionDataItem {
  display?: boolean /** @default 'true' */
  key?: string
}
interface ITableDropMenuProps {
  className?: string
  style?: React.CSSProperties
  text?: React.ReactNode
  textClassName?: string
  textStyle?: React.CSSProperties
  tooltip?: boolean | React.ReactNode /** 文字部分tooltip */
  // clickableText?: boolean
  showText?: boolean /** @default 'true' */
  showMenu?: boolean /** @default 'true' */
  defaultMenuIcon?: string /** @default 'more_vert' */
  defaultButtonProps?: Partial<Pick<ButtonProps, 'className'|'style'|'shape'|'size'|'disabled'|'icon'>>
  menuData?: ITableDropMenuItem[]
  onMenuClick?: (data: ITableDropMenuItem) => void /** 点击没有action菜单项会触发此事件 */
  oldMenuData?: React.ReactElement /** @deprecated 后续将废弃此接口  旧的TableDropMenu 传入内容将自动拼接到menuData上 */
  organizationId?: string /** 权限校验时组织 */
  permissionType?: string /** 权限校验时类型 */
  permission?: IBootPermissionProps /** 全局权限配置 */
  permissionMenu?: IBootPermissionProps /** 菜单权限配置 会覆盖全局 */
  permissionText?: IBootPermissionProps /** 文字部分操作权限配置 会覆盖全局 */

}
/**
 * 表格中带下拉菜单的文字
 * @param param0
 * @returns
 */
const TableDropMenu: React.FC<ITableDropMenuProps> = ({
  text, className, style, menuData: propsMenuData, oldMenuData, showMenu: propsShowMenu, organizationId, showText = true, defaultButtonProps,
  permissionType, tooltip, permissionText, textClassName, textStyle, permission, permissionMenu, onMenuClick, defaultMenuIcon = 'more_vert',
}) => {
  const prefixCls = 'c7n-agile-table-drop-menu';
  // 渲染文本
  const renderText = () => {
    const finalText = text;
    return tooltip ? <Tooltip placement="topLeft" title={typeof (tooltip) !== 'boolean' ? tooltip : text}>{finalText}</Tooltip> : finalText;
  };// cursor: 'pointer'
  const handleMenuClick = useCallback((data: ITableDropMenuItem) => {
    onMenuClick && onMenuClick(data);
  }, [onMenuClick]);
  const menuData = useMemo(() => {
    let newMenuData = propsMenuData || [];
    if (oldMenuData && React.isValidElement(oldMenuData)) {
      const menuProps = pick<any>(oldMenuData.props, ['children', 'onClick']);

      const children = React.Children.map(menuProps.children, (item: any) => ({
        action: () => menuProps.onClick({ key: item.key }),
        text: item.props?.children,

      })) || [];
      newMenuData.push(...children);
    }
    newMenuData = newMenuData.filter((i) => i && Object.keys(i).length > 0 && (typeof (i.display) === 'undefined' || i.display))
      .map((i) => ({ ...i, action: i.action || (() => handleMenuClick(i)) }));
    return newMenuData.length > 0 ? newMenuData : undefined;
  }, [handleMenuClick, oldMenuData, propsMenuData]);
  const showMenu = useMemo(() => {
    if (typeof (propsShowMenu) === 'boolean') {
      return propsShowMenu;
    }
    return !!menuData?.length;
  }, [menuData?.length, propsShowMenu]);
  return (
    <div
      className={classNames(prefixCls, className)}
      style={style}
    >
      {showText && (
        <span className={classNames(`${prefixCls}-text`, textClassName)} style={textStyle}>
          {permissionText
            ? (
              <Permission
                type={permissionType}
                organizationId={organizationId}
                {...permission}
                {...permissionText}
              >
                {renderText()}
              </Permission>
            )
            : renderText()}
        </span>
      )}
      {showMenu && (
        <Permission
          type={permissionType}
          organizationId={organizationId}
          {...permission}
          {...permissionMenu}
        >
          <Action
            data={menuData}
            type={permissionType}
            organizationId={organizationId}
            icon={defaultMenuIcon}
            {...defaultButtonProps}
          />
        </Permission>
      )}
    </div>
  );
};

export default TableDropMenu;
