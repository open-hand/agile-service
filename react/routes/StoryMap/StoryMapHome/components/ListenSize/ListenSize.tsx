import React, { useEffect, useState } from 'react';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { useSize } from 'ahooks';
import { BasicTarget } from 'ahooks/lib/utils/dom';

const ListenSize: React.FC = () => {
  const [containerEle, setContainerEle] = useState<Element | null>(null);
  const [tableEle, setTableEle] = useState<Element | null>(null);

  useEffect(() => {
    setContainerEle(document.getElementsByClassName('page-content')[0]);
    setTableEle(document.getElementsByClassName('c7nagile-StoryMapBody-table')[0]);
  }, [containerEle]);

  const containerSize = useSize(containerEle as BasicTarget);
  const tableSize = useSize(tableEle as BasicTarget);

  useEffect(() => {
    StoryMapStore.setTableOverflow({ tableWidth: tableSize.width, containerWidth: containerSize.width });
  }, [containerSize.width, tableSize.width]);

  return (
    <div />
  );
};

export default ListenSize;
