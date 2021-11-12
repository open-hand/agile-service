import React, {
  useEffect, ReactElement, useRef, MutableRefObject, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { forEach, isEmpty } from 'lodash';
import { Icon } from 'choerodon-ui/pro';
import Tree from '@/components/tree';
import { useWorkGroupStore } from '@/routes/work-group/stores';
import { EditFormDataProps, workGroupApi } from '@/api/WorkGroup';
import { GroupItem } from '@/routes/work-group/types';
import Styles from './index.less';

export default observer(() => {
  const treeRef: MutableRefObject<ReactElement | undefined> = useRef();
  const {
    mainStore,
    ROOT_ID,
    NOT_ASSIGN_ID,
    tableDs,
  } = useWorkGroupStore();

  // @ts-ignore
  useEffect(() => mainStore.setTreeRef(treeRef), [treeRef]);

  const getExtraTreeNode = useMemo(() => (
    <div
      role="none"
      onClick={() => setSelected(mainStore.getNotAssignItem)}
      className={Styles.notAssign}
    >
      <span className={Styles.notAssignDot} />
      <Icon type="folder_open" />
      <span>未分配工作组</span>
    </div>
  ), [mainStore.getNotAssignItem]);

  const handleCreate = async (value: string, parentId: string | number): Promise<object> => {
    const data = {
      parentId: parentId === ROOT_ID ? 0 : parentId ?? 0,
      name: value,
    };
    try {
      const result = await workGroupApi.createWorkGroup(data);
      if (parentId === 0) {
        mainStore.addRootItem(result.folderId);
      }
      const newData = {
        id: result.folderId,
        children: [],
        data: result,
      };
      // 增添至数组内，防止导入后对树内数据搜索无数据
      mainStore.addTreeFolder(newData);
      return {
        id: result.id,
        data: {
          parentId,
          name: value,
          objectVersionNumber: result.objectVersionNumber,
        },
      };
    } catch (e) {
      return {};
    }
  };

  const handleEdit = async (newName: string, item: GroupItem): Promise<object> => {
    const { objectVersionNumber } = item.data || {};
    const data: EditFormDataProps = {
      id: item.id,
      objectVersionNumber,
      name: newName,
    };
    try {
      const result = await workGroupApi.editWorkGroup(data);
      return {
        data: {
          ...item.data,
          name: result.name,
          objectVersionNumber: result.objectVersionNumber,
        },
      };
    } catch (e) {
      return {};
    }
  };

  const handleDrag = async (sourceItem: GroupItem, destination: GroupItem): Promise<object> => {
    if (sourceItem.parentId === destination.parentId && sourceItem.index === destination.index) {
      return {};
    }
    // @ts-ignore
    const { treeData } = treeRef.current;
    const parent = treeData.items[destination.parentId];
    const { index = parent.children.length } = destination;
    const isLast = parent.children.length === index;
    let outSetIndex = index;
    // 树从上往下拖拽排序，且并非最后一个
    if (sourceItem.parentId === destination.parentId && sourceItem.index < index && isLast) {
      outSetIndex = index + 1;
    }
    const outSetId = parent.children[outSetIndex];
    const data = {
      workGroupId: sourceItem.id,
      before: !isLast,
      outSetId,
    };
    try {
      const parentId = destination.parentId === ROOT_ID ? 0 : destination.parentId;
      const res = await workGroupApi.moveWorkGroup(data, parentId);
      return {
        data: {
          ...sourceItem.data,
          rank: res?.rank,
          parentId: destination.parentId,
          objectVersionNumber: res.objectVersionNumber,
        },
      };
    } catch (e) {
      return {};
    }
  };

  const handleDelete = async (item: GroupItem): Promise<void> => {
    try {
      await workGroupApi.deleteWorkGroup(item.id);
      // 只移除跟节点，作用是删除目录后可以正确判断是不是没目录了，来显示空插画
      mainStore.removeRootItem(item.id);
    } catch (e) {
      // return false;
    }
  };

  const setSelected = (item: GroupItem) => {
    mainStore.setSelectedMenu(item);
    mainStore.setSelectedMenu({ ...item, ...item.data || {} });
    if (item.id) {
      tableDs.setQueryParameter('workGroupId', item.id);
      tableDs.query();
    }
    if (isEmpty(item)) {
      tableDs.removeAll();
    }
  };

  return (
    <>
      <Tree
        ref={treeRef}
        search={mainStore.getSearchValue}
        onSearchChange={(value: string) => (mainStore.setSearchValue(value))}
        data={mainStore.getTreeData}
        onCreate={handleCreate}
        onEdit={handleEdit}
        onDelete={handleDelete}
        afterDrag={handleDrag}
        selected={mainStore.getSelectedMenu}
        setSelected={setSelected}
        updateItem={setSelected}
        isDragEnabled={!mainStore.getSearchValue}
        treeNodeProps={{
          enableAddFolder: true,
          enableAction: (item: GroupItem) => item.id !== ROOT_ID,
          titleSuffix: (item: GroupItem) => ` (${item.data?.userCount})`,
        }}
        editNodeProps={{
          placeholder: '请输入工作组名称',
          maxLength: 32,
        }}
        getDeleteTitle={(item: GroupItem) => `删除工作组|确认删除“${item?.name}”工作组？删除后，该工作组的成员不会被删除，工作组下的子工作组将会一并删除。`}
        extraTreeNode={getExtraTreeNode}
      />
    </>
  );
});
