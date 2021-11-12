import { MutableRefObject, ReactElement, useMemo } from 'react';
import { useLocalStore } from 'mobx-react-lite';
import {
  find, includes, pull, isEmpty, remove,
} from 'lodash';
import { workGroupApi } from '@/api/WorkGroup';
import { GroupItem } from '@/routes/work-group/types';

interface Props {
  ROOT_ID: string,
  NOT_ASSIGN_ID: string,
}

interface TreeDataProps {
  rootIds: Array<string>,
  treeFolder: Array<GroupItem>,
}

export default function useStore({ ROOT_ID, NOT_ASSIGN_ID }: Props) {
  const store = useLocalStore(() => ({
    selectedMenu: { id: ROOT_ID },
    get getSelectedMenu() {
      return this.selectedMenu;
    },
    setSelectedMenu(data: object) {
      this.selectedMenu = data;
    },

    rootIds: [],
    get getRootIds() {
      return this.rootIds;
    },
    setRootIds(data: Array<string>) {
      this.rootIds = data;
    },

    addRootItem(folderId: string) {
      this.rootIds.push(folderId);
    },
    removeRootItem(folderId: string) {
      pull(this.rootIds, folderId);
    },
    addTreeFolder(item: object) {
      this.treeData.treeFolder.push(item);
    },

    isLoading: false,
    get getIsLoading() {
      return this.isLoading;
    },
    setIsLoading(flag: boolean) {
      this.isLoading = flag;
    },

    searchValue: '',
    get getSearchValue() {
      return this.searchValue;
    },
    setSearchValue(data: string) {
      this.searchValue = data;
    },

    notAssignItem: {},
    get getNotAssignItem() {
      return this.notAssignItem;
    },
    setNotAssignItem(data: GroupItem) {
      this.notAssignItem = data;
    },

    treeData: {
      rootIds: [],
      treeFolder: [],
    },
    get getTreeData() {
      return this.treeData;
    },
    setTreeData(treeData: TreeDataProps) {
      let expandedKeys: Array<string | never> = [];
      // @ts-ignore
      const { flattenedTree } = this.treeRef ? this.treeRef.current : {};
      if (flattenedTree) {
        expandedKeys = flattenedTree.map((node: { item: object; }) => node.item)
          .filter((item: { isExpanded: boolean; }) => item.isExpanded)
          .map((item: { id: string; }) => item.id);
      }
      const { rootIds, treeFolder } = treeData;
      // 选中之前选中的
      let selectedId = this.selectedMenu?.id;
      const hasSelectedItem = treeFolder.some(({ id }) => id === selectedId);
      if (!hasSelectedItem) {
        // eslint-disable-next-line prefer-destructuring
        selectedId = ROOT_ID;
      }
      this.treeData = {
        rootIds: isEmpty(treeFolder) ? [] : [ROOT_ID],
        treeFolder: treeFolder.filter((folder) => !(!folder.id && folder.parentId === 0)).map((folder) => {
          const {
            id, parentId, expanded, children, ...other
          } = folder;
          // 组织信息：id和parentId都为空
          const newId = id ?? ROOT_ID;
          const newParentId = parentId === 0 ? ROOT_ID : (parentId ?? 0);
          return ({
            id: newId,
            children: newId === ROOT_ID ? rootIds : children || [],
            isExpanded: expanded || includes(expandedKeys, newId),
            selected: newId === selectedId,
            ...other,
            parentId: newParentId,
            data: {
              id: newId,
              parentId: newParentId,
              ...other,
            },
          });
        }),
      };
      this.setRootIds([ROOT_ID]);
      if (selectedId) {
        const selectedItem = find(this.treeData.treeFolder, { id: selectedId }) || {};
        this.setSelectedMenu(selectedItem);
      }
    },
    async loadTreeData() {
      try {
        this.setIsLoading(true);
        const res = await workGroupApi.loadWorkGroup();
        this.setIsLoading(false);
        const treeFolder = res.workGroupVOS || [];
        // 过滤未分配工作组（因为未分配工作组不能拖住，不能当做树节点）
        const notAssignItem = remove(treeFolder, (item: GroupItem) => !item.id && item.parentId === 0);
        const newData = {
          rootIds: res.rootIds || [],
          treeFolder,
        };
        this.setTreeData(newData);
        this.setNotAssignItem({
          ...notAssignItem || {},
          id: NOT_ASSIGN_ID,
          parentId: ROOT_ID,
        });
      } catch (e) {
        // aa
      }
    },
  }));

  // 让treeRef不成为可观察对象
  const obj = useMemo(() => ({
    treeRef: null,
    get getTreeRef() {
      return this.treeRef;
    },
    setTreeRef(data: MutableRefObject<ReactElement | undefined>) {
      this.treeRef = data;
    },
  }), []);
  Object.assign(store, obj);

  return store;
}

export type StoreProps = ReturnType<typeof useStore>;
