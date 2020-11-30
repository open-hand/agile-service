import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { useInViewport } from 'ahooks';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { BasicTarget } from 'ahooks/lib/utils/dom';

interface Props {
  id: string
}

const ListenRowInViewport: React.FC<Props> = ({ id }) => {
  const [rowCellEle, setRowCellEle] = useState<BasicTarget>();

  useEffect(() => {
    const el = document.getElementsByClassName(`row-${id}`)[0];
    setRowCellEle(el as BasicTarget);
  }, [id]);

  const inViewport = useInViewport(rowCellEle as HTMLElement);

  useEffect(() => {
    StoryMapStore.setRowInViewportMap(id, inViewport);
  }, [id, inViewport]);

  return null;
};

export default observer(ListenRowInViewport);
