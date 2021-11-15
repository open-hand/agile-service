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
        treeFolder: treeFolder.map((folder) => {
          const {
            id, parentId, expanded, children, ...other
          } = folder;
          // 组织信息：id和parentId都为空
          // 未分配工作组信息：id为空和parentId为0
          const newId = id ?? (parentId === 0 ? NOT_ASSIGN_ID : ROOT_ID);
          const newParentId = parentId === 0 ? ROOT_ID : (parentId ?? 0);
          const newChildren = newId === ROOT_ID ? rootIds.concat([NOT_ASSIGN_ID]) : children || [];
          return ({
            id: newId,
            children: newChildren,
            isExpanded: expanded || includes(expandedKeys, newId),
            selected: newId === selectedId,
            parentId: newParentId,
            ...other,
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
        const newData = {
          rootIds: res.rootIds || [],
          treeFolder: res.workGroupVOS || [],
        };
        this.setTreeData(newData);
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
