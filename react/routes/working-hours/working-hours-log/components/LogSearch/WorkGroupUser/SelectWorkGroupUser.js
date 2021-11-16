import React, {
  useEffect, useCallback, useRef, useState,
} from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { CheckBox } from 'choerodon-ui/pro';
import Tree from '@/components/tree';
// import CheckBox from './CheckBox';
import { autoSelect } from './utils';
import UserList from './UserList';
import { useSelectUserStore } from './stores';
import styles from './SelectWorkGroupUser.less';

function SelectIssue() {
  const [updateCount, setUpdateCount] = useState(0);
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
    console.log('treeMap, checked, item:');
    console.log(toJS(treeMap), checked, item, toJS(item));
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
    // setUpdateCount(updateCount + 1);
    console.log(treeMap.get(item.id));
  }, [treeMap, selectUserStore, userListDs, folderId]);
  const renderTreeNode = (node, { item }) => {
    console.log('renderNode', item, toJS(treeMap.get(item.id)), treeMap.get(item.id).checked);
    return (
    // <div className={styles.treeNode}>
    //   <CheckBox
    //     item={treeMap.get(item.id)}
    //     onChange={handleCheckChange}
    //   />
    //   <div style={{ flex: 1, overflow: 'hidden' }}>
    //     {node}
    //   </div>
    // </div>
      <div className={styles.treeNode}>
        <CheckBox
          onChange={(value) => handleCheckChange(value, item)}
          indeterminate={treeMap.get(item.isIndeterminate) || false}
          checked={treeMap.get(item.id).checked || false}
        />
        <div style={{ flex: 1, overflow: 'hidden' }}>
          {node}
        </div>
      </div>
    );
  };

  console.log('render');

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
