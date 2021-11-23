interface GroupItem {
  id: string,
  index: number,
  name: string,
  userCount: number,
  parentId: string | 0,
  children: GroupItem[],
  expanded: boolean,
  data: {
    name: string,
    objectVersionNumber: number,
    userCount: number,
  },
  objectVersionNumber: number
}

export {
  GroupItem,
};
