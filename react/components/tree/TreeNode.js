import React, {
  Fragment, useCallback, useEffect, useRef,
} from 'react';
import classNames from 'classnames';
import {
  Icon, Button, TextField,
  Menu, Dropdown, Tooltip,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Styles from './index.less';

function callFunction(prop, ...args) {
  if (typeof prop === 'function') {
    return prop(...args);
  }
  return prop;
}
const defaultProps = {
  enableAddFolder: false,
  enableAction: true,
};
const prefix = 'c7ntest-tree';

const getAction = (item, menuItems, enableAddFolder, onMenuClick, enableAction) => {
  const menus = menuItems ? callFunction(menuItems, item) : [
    <Menu.Item key="rename">
      重命名
    </Menu.Item>,
    <Menu.Item key="delete">
      删除
    </Menu.Item>];
  const menu = (
    <Menu onClick={(target) => { onMenuClick(item, target); }}>
      {menus}
    </Menu>
  );
  return (
    <div key={item.id} role="none" onClick={(e) => { e.stopPropagation(); }} className={Styles.action}>
      {(callFunction(enableAddFolder, item)) && <Icon type="create_new_folder" style={{ marginRight: 6 }} onClick={() => { onMenuClick(item, { key: 'add' }); }} />}
      {(callFunction(enableAction, item) && menus) && (
        <Dropdown overlay={menu} trigger={['click']}>
          <Button funcType="flat" icon="more_vert" size="small" />
        </Dropdown>
      )}
    </div>
  );
};

function TreeNode(props) {
  const {
    provided, onSelect, path, item, onExpand, onCollapse, onMenuClick,
    onCreate, search, onEdit, enableAction, menuItems, enableAddFolder,
    getFolderIcon, editNodeProps, titleSuffix,
  } = props;
  const getIcon = useCallback(() => {
    const expandIcon = (
      <Icon
        type="baseline-arrow_right"
        className={classNames(Styles.icon, { [Styles.expanded]: item.isExpanded })}
        onClick={(e) => {
          e.stopPropagation();
          if (item.isExpanded) {
            onCollapse(item.id);
          } else {
            onExpand(item.id);
          }
        }}
      />
    );
    const defaultIcon = (
      <Icon
        type={item.isExpanded ? 'folder_open2' : 'folder_open'}
        className={classNames(Styles.folder, Styles.primary)}
      />
    );
    const folderIcon = getFolderIcon ? callFunction(getFolderIcon, item, defaultIcon) : defaultIcon;
    if (item.children && item.children.length > 0) {
      return (
        <>
          {expandIcon}
          {folderIcon}
        </>
      );
    }
    return (
      <>
        <span style={{
          display: 'inline-block',
          visibility: 'hidden',
          width: 22,
          justifyContent: 'center',
          cursor: 'pointer',
        }}
        >
          &bull;
        </span>
        {folderIcon}
      </>
    );
  }, [getFolderIcon, item, onCollapse, onExpand]);
  const onSave = (e) => {
    if (item.id === 'new') {
      onCreate(e.target.value, path, item);
    } else {
      onEdit(e.target.value, item);
    }
  };
  const renderEditing = () => (
    <div
      role="none"
      className={classNames(Styles.item, Styles.itemEditing)}
    >
      <TextField
        placeholder="请输入目录名称"
        style={{ width: '100%' }}
        maxLength={20}
        defaultValue={item.data.name}
        onBlur={onSave}
        autoFocus
        valueChangeAction="input"
        {...editNodeProps || {}}
      />
    </div>
  );
  const renderTitle = () => {
    const { name } = item.data;
    const index = name.indexOf(search);
    const beforeStr = name.substr(0, index);
    const afterStr = name.substr(index + search.length);
    const result = index > -1 ? (
      <span>
        {beforeStr}
        <span style={{ color: '#f50' }}>{search}</span>
        {afterStr}
      </span>
    ) : name;
    return <Tooltip title={result}>{result}</Tooltip>;
  };
  const renderContent = () => (
    <div
      className={Styles.itemWrap}
    >
      <div
        role="none"
        className={classNames(Styles.item, { [Styles.selected]: item.selected })}
        onClick={() => { onSelect(item); }}
      >
        <span className={Styles.prefix}>{getIcon(item, onExpand, onCollapse)}</span>
        <span className={Styles.title}>
          {renderTitle()}
          {callFunction(titleSuffix, item)}
        </span>
        {item.data.openType === 'ding_talk' && <div className={Styles.dingflag}>钉钉同步 </div>}
        {(callFunction(enableAction, item) || callFunction(enableAddFolder, item)) && getAction({ ...item, path }, menuItems, enableAddFolder, onMenuClick, enableAction)}
      </div>
    </div>
  );
  return (
    <div
      data-id={item.id}
      ref={(instance) => {
        provided.innerRef(instance);
      }}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
    >
      {item.isEditing ? renderEditing() : renderContent()}
    </div>
  );
}
TreeNode.defaultProps = defaultProps;
export default observer(TreeNode);
