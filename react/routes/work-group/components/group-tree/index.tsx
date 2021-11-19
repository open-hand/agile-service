import React, {
  useEffect, ReactElement, useRef, MutableRefObject, useCallback, ReactNode,
} from 'react';
import { observer } from 'mobx-react-lite';
import { isEmpty } from 'lodash';
import Tree from '@/components/tree';
import { useWorkGroupStore } from '@/routes/work-group/stores';
import { EditFormDataProps, workGroupApi } from '@/api';
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

  const refresh = useCallback(() => {
    mainStore.loadTreeData();
  }, []);

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

  const handleBeforeDrag = (sourceItem: GroupItem, destination: GroupItem) => {
    // @ts-ignore
    const { treeData } = treeRef.current;
    const { parentId, index } = destination;
    const parent = treeData.items[parentId];
    const isLast = parent.children.length === index;
    if ([0, '0', NOT_ASSIGN_ID].includes(parentId) || (parentId === ROOT_ID && isLast)) {
      return false;
    }
    return true;
  };

  const handleDrag = async (sourceItem: GroupItem, destination: GroupItem): Promise<object> => {
    if (sourceItem.parentId === destination.parentId && sourceItem.index === destination.index) {
      return {};
    }
    // @ts-ignore
    const { treeData } = treeRef.current;
    const parent = treeData.items[destination.parentId];
    const { index = parent.children.length } = destination;
    // 如果是组织下的直接树节点，计算是否是最后一个时需要考虑未分配工作组
    const isLast = parent.children.length === (destination.parentId === ROOT_ID ? index + 2 : index + 1);
    // parent.children获取到的值是按照拖拽后的排序
    const outSetIndex = isLast ? index - 1 : index + 1;
    const outSetId = parent.children[outSetIndex] ?? 0;
    const data = {
      workGroupId: sourceItem.id,
      before: !isLast,
      outSetId,
    };
    try {
      const parentId = destination.parentId === ROOT_ID ? 0 : destination.parentId;
      const res = await workGroupApi.moveWorkGroup(data, parentId);
      refresh();
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
    await workGroupApi.deleteWorkGroup(item.id);
    // 只移除跟节点，作用是删除目录后可以正确判断是不是没目录了，来显示空插画
    mainStore.removeRootItem(item.id);
    refresh();
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

  const renderTreeNode = (treeNode: ReactNode, { item }: { item: GroupItem }) => {
    if (item.id === NOT_ASSIGN_ID) {
      return (
        <div className={Styles.treeItemWrap}>
          <div className={Styles.treeItemDot} />
          {treeNode}
        </div>
      );
    }
    return treeNode;
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
        beforeDrag={handleBeforeDrag}
        afterDrag={handleDrag}
        selected={mainStore.getSelectedMenu}
        setSelected={setSelected}
        updateItem={setSelected}
        renderTreeNode={renderTreeNode}
        isDragEnabled={(item: GroupItem) => ![ROOT_ID, NOT_ASSIGN_ID].includes(item.id)}
        treeNodeProps={{
          enableAddFolder: (item: GroupItem) => item.id !== NOT_ASSIGN_ID,
          enableAction: (item: GroupItem) => ![ROOT_ID, NOT_ASSIGN_ID].includes(item.id),
          titleSuffix: (item: GroupItem) => ` (${item.data?.userCount ?? 0})`,
        }}
        editNodeProps={{
          placeholder: '请输入工作组名称',
          maxLength: 32,
        }}
        getDeleteTitle={(item: GroupItem) => `删除工作组|确认删除“${item.data?.name}”工作组？删除后，该工作组的成员不会被删除，工作组下的子工作组将会一并删除。`}
      />
    </>
  );
});
