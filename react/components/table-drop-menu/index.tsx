import React, { useCallback, useMemo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import { Action, Permission } from '@choerodon/boot';
import { pick } from 'lodash';
import classNames from 'classnames';
import type { ButtonProps } from 'choerodon-ui/lib/button';
import type { IBootActionDataItem, IBootPermissionProps } from '../../common/types';
import './index.less';

export interface ITableDropMenuItem extends IBootActionDataItem {
  display?: boolean /** @default 'true' */
  key?: string
}
interface ITableDropMenuProps {
  className?: string
  style?: React.CSSProperties
  text?: React.ReactNode
  textClassName?: string
  /**
   *  文本点击事件 当配置时 则启动文本可点击
   */
  onTextClick?: React.MouseEventHandler<HTMLSpanElement>
  textStyle?: React.CSSProperties
  tooltip?: boolean | React.ReactNode /** 文字部分tooltip */
  // clickableText?: boolean
  /** @default 'true' */
  showText?: boolean

  onTextClick?:(event:any)=>void
  /**
   * 是否展示菜单，当菜单列表为空时不会展示
   * 设置为 `true` 则会常展示
   *  @default 'true'
   *  */
  showMenu?: boolean
  defaultMenuIcon?: string /** @default 'more_vert' */
  defaultButtonProps?: Partial<Pick<ButtonProps, 'className' | 'style' | 'shape' | 'size' | 'disabled' | 'icon'>>
  menuData?: ITableDropMenuItem[]
  /** 点击没有action菜单项会触发此事件 */
  onMenuClick?: (data: ITableDropMenuItem) => void
  /** @deprecated 后续将废弃此接口  旧的TableDropMenu 传入内容将自动拼接到menuData上 */
  oldMenuData?: React.ReactElement
  /** 权限校验时组织 */
  organizationId?: string
  /** 权限校验时类型 */
  permissionType?: string
  /** 全局权限配置 */
  permission?: IBootPermissionProps
  /** 菜单权限配置 会覆盖全局 */
  permissionMenu?: IBootPermissionProps
  /** 文字部分操作权限配置 会覆盖全局 */
  permissionText?: IBootPermissionProps

}
/**
 * 表格中带下拉菜单的文字
 * @param param0
 * @returns
 */
const TableDropMenu: React.FC<ITableDropMenuProps> = ({
  text, className, style, menuData: propsMenuData, oldMenuData, showMenu: propsShowMenu, organizationId, showText = true, defaultButtonProps,
  permissionType, tooltip, permissionText, textClassName, textStyle, permission, permissionMenu, onMenuClick, defaultMenuIcon = 'more_vert', onTextClick,
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
        <span role="none" className={classNames(`${prefixCls}-text`, { [`${prefixCls}-text-click`]: onTextClick }, textClassName)} style={textStyle} onClick={onTextClick}>
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
