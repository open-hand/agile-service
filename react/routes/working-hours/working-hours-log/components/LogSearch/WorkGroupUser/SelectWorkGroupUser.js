import React, {
  useEffect, useCallback, useRef,
} from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import Tree from '@/components/tree';
import CheckBox from './CheckBox';
import { autoSelect } from './utils';
import UserList from './UserList';
import { useSelectUserStore } from './stores';
import styles from './SelectWorkGroupUser.less';

function SelectIssue() {
  const treeRef = useRef();
  const { selectUserStore, userListDs } = useSelectUserStore();
  useEffect(() => {
    selectUserStore.setTreeRef(treeRef);
  }, [selectUserStore]);
  const { currentCycle, treeData, treeMap } = selectUserStore;
  const { id: folderId } = currentCycle;
  const setSelected = useCallback((item) => {
    selectUserStore.setCurrentCycle(item);
  }, [selectUserStore]);
  const handleCheckChange = useCallback((checked, item) => {
    selectUserStore.handleCheckChange(checked, item.id);
    // 如果选中，跑一遍自动选中
    if (checked && userListDs) {
      if (folderId === item.id) {
        autoSelect(userListDs, treeMap);
      } else {
        autoSelect(userListDs, treeMap);
      }
    } else if (!checked && userListDs) {
      if (folderId === item.id) {
        autoSelect(userListDs, treeMap);
      } else {
        autoSelect(userListDs, treeMap);
      }
    }
  }, [selectUserStore, userListDs, folderId, treeMap]);
  const renderTreeNode = useCallback((node, { item }) => (
    <div className={styles.treeNode}>
      <CheckBox
        item={treeMap.get(item.id)}
        onChange={handleCheckChange}
      />
      <div style={{ flex: 1, overflow: 'hidden' }}>
        {node}
      </div>
    </div>
  ), [handleCheckChange, treeMap]);
  console.log('treeData:');
  console.log(toJS(treeData));
  return (
    <div className={styles.selectWorkGroupUser}>
      <div className={styles.tree}>
        <Tree
          ref={treeRef}
          data={treeData}
          isDragEnabled={false}
          selected={currentCycle}
          setSelected={setSelected}
          treeNodeProps={{
            enableAction: false,
          }}
          renderTreeNode={renderTreeNode}
          treeWrapCls={styles.treeWrapCls}
          topCls={styles.topCls}
          scrollCls={styles.scrollCls}
        />
      </div>
      <div className={styles.list}>
        <UserList />
      </div>
    </div>
  );
}
export default observer(SelectIssue);
