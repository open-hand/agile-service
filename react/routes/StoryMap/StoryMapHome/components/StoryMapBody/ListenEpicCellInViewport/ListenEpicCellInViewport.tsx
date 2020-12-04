import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { useInViewport } from 'ahooks';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { BasicTarget } from 'ahooks/lib/utils/dom';

interface Props {
  epicId: string
}

const ListenEpicCellInViewport: React.FC<Props> = ({ epicId }) => {
  const [epicCellEle, setEpicCellEle] = useState<BasicTarget>();

  useEffect(() => {
    const el = document.getElementsByClassName(`epicCell-${epicId}`)[0];
    setEpicCellEle(el as BasicTarget);
  }, [epicId]);

  const inViewport = useInViewport(epicCellEle as HTMLElement);

  useEffect(() => {
    StoryMapStore.setEpicInViewportMap(epicId, inViewport);
  }, [epicId, inViewport]);

  return null;
};

export default observer(ListenEpicCellInViewport);
