import React, {} from 'react';
import { observer } from 'mobx-react-lite';
import { Button } from 'choerodon-ui/pro';
import useFullScreen from '@/common/useFullScreen';

const BoardFullScreen = (props) => {
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.body, () => {}, 'c7n-scrumboard-fullScreen');
  return (
    <Button
      {...props}
      onClick={() => { toggleFullScreen(); }}
      icon={isFullScreen ? 'fullscreen_exit' : 'zoom_out_map'}
    >
      {isFullScreen ? '退出全屏' : '全屏'}
    </Button>
  );
};

export default observer(BoardFullScreen);
