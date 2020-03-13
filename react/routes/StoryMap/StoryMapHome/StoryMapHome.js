/* eslint-disable no-nested-ternary */
import React, {
  Fragment, useEffect, useRef,
} from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui';
import { DragDropContextProvider } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import { observer } from 'mobx-react-lite';
import HeaderLine from '@/common/Headerline';
import Minimap from '../../../components/MiniMap';
import Empty from '../../../components/Empty';
import epicPic from '../../../assets/image/emptyStory.svg';
import Loading from '../../../components/Loading';
import StoryMapBody from './components/StoryMapBody';
import SideIssueList from './components/SideIssueList';
import SwitchSwimLine from './components/SwitchSwimLine';
import CreateVersion from './components/CreateVersion';
import CreateEpicModal from './components/CreateEpicModal';
import IssueDetail from './components/IssueDetail';
import StoryMapStore from '../../../stores/project/StoryMap/StoryMapStore';
import useFullScreen from './useFullScreen';
import './StoryMapHome.less';

const HEX = {
  'c7nagile-StoryMap-EpicCard': '#D9C2FB',
  'c7nagile-StoryMap-StoryCard': '#AEE9E0',
  business: '#BCC6FF',
  enabler: '#FEA',
};

const StoryMapHome = observer(() => {
  const handleRefresh = () => {
    StoryMapStore.getStoryMap();
  };
  const ref = useRef(null);
  StoryMapStore.setMiniMapRef(ref);
  // useEffect(() => {
  //   StoryMapStore.setMiniMapRef(ref);
  // }, [ref]);
  useEffect(() => {
    handleRefresh();
    return () => { StoryMapStore.clear(); };
  }, []);
  const handleOpenIssueList = () => {
    StoryMapStore.toggleSideIssueListVisible(!StoryMapStore.sideIssueListVisible);
  };
  const handleCloseIssueList = () => {
    setTimeout(() => {
      StoryMapStore.toggleSideIssueListVisible(false);
    });
  };

  const handleCreateEpicClick = () => {
    StoryMapStore.setCreateEpicModalVisible(true);
  };
  const handleCreateVersion = (version) => {
    StoryMapStore.afterCreateVersion(version);
    document.getElementsByClassName('minimap-container-scroll')[0].scrollTop = 0;
  };

  const handleCreateEpic = (newEpic) => {
    StoryMapStore.setCreateEpicModalVisible(false);
    StoryMapStore.afterCreateEpicInModal(newEpic);
  };


  const onFullScreenChange = (isFullScreen) => {
    StoryMapStore.setIsFullScreen(!!isFullScreen);
  };


  const renderChild = ({
    width, height, left, top, node,
  }) => {
    let classNameFound = null;
    node.classList.forEach((className) => {
      if (HEX[className]) {
        classNameFound = className;
      }
    });

    return (
      <div
        style={{
          position: 'absolute',
          width,
          height,
          left,
          top,
          backgroundColor: HEX[classNameFound],
        }}
      />
    );
  };

  const handleIssueRefresh = () => {
    handleRefresh();
  };
  /**
   * 问题宽度localStorage.getItem('agile.EditIssue.width')
   * @param {*} width 
   */
  const setIssueWidth = (width) => {
    if (ref.current) {
      ref.current.source.style.width = `calc(100% - ${width || 400}px)`;
    }
  };

  const { loading, selectedIssueMap } = StoryMapStore;

  /**
   * 打开问题详情时设置样式 用以显示全部地图
   */
  useEffect(() => {
    if (ref.current && selectedIssueMap.size) {
      ref.current.source.style.width = `calc(100% - ${localStorage.getItem('agile.EditIssue.width')})`;
    } else if (ref.current) {
      ref.current.source.style.width = '';
    }
  }, [selectedIssueMap.size]);

  const isEmpty = StoryMapStore.getIsEmpty;
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.getElementsByClassName('c7nagile-StoryMap')[0], onFullScreenChange);
  return (
    <Page
      className="c7nagile-StoryMap"
      service={[
        'agile-service.scheme.queryIssueTypesWithStateMachineIdByProjectId',
        'agile-service.product-version.queryNameByOptions',
        'agile-service.scheme.queryByOrganizationIdList',
        'agile-service.scheme.queryStatusByProjectId',
        'agile-service.story-map.queryStoryMapDemand',
        'agile-service.issue.createIssue',
        'agile-service.field-value.createFieldValuesWithQuickCreate',
        'agile-service.story-map.storyMapMove',
        'agile-service.story-map.queryStoryMap',
        'agile-service.scheme.queryIssueTypesWithStateMachineIdByProjectId',
        'agile-service.scheme.queryByOrganizationIdList',
      ]}
    >
      <Header title="故事地图">
        {/* <Button
          icon="refresh"
          onClick={handleRefresh}
        >
            刷新
        </Button> */}
        {isEmpty && !loading ? <Button onClick={handleCreateEpicClick} icon="playlist_add">创建史诗</Button> : null}
        {!StoryMapStore.isFullScreen && (
          <Button
            icon="view_module"
            onClick={handleOpenIssueList}
          >
            需求池
          </Button>
        )}
        <Button onClick={toggleFullScreen} icon={isFullScreen ? 'fullscreen_exit' : 'zoom_out_map'}>
          {isFullScreen ? '退出全屏' : '全屏'}
        </Button>
        <HeaderLine />
        <SwitchSwimLine />
      </Header>
      <Breadcrumb />
      <Content style={{
        padding: 0, borderTop: '1px solid #D8D8D8',
      }}
      >
        <Loading loading={loading} />
        {!isEmpty ? (
          <Fragment>
            <Minimap ref={ref} disabledVertical width={300} height={40} showHeight={300} className="c7nagile-StoryMap-minimap" selector=".minimapCard" childComponent={renderChild}>
              <StoryMapBody />
            </Minimap>
          </Fragment>
        ) : (
          loading ? null : (
              // eslint-disable-next-line react/jsx-indent
              <Empty
                style={{ background: 'white', height: 'calc(100% + 120px)', marginTop: -120 }}
                pic={epicPic}
                title="欢迎使用敏捷用户故事地图"
                description={(
                  <Fragment>
                    用户故事地图是以史诗为基础，根据版本控制进行管理规划
                  </Fragment>
                )}
              />
          )
        )}
        <SideIssueList handleClickOutside={handleCloseIssueList} eventTypes={['click']} />
        <CreateVersion onOk={handleCreateVersion} />
        <CreateEpicModal onOk={handleCreateEpic} />
        <IssueDetail refresh={handleIssueRefresh} isFullScreen={isFullScreen} onChangeWidth={setIssueWidth} />
      </Content>
    </Page>
  );
});

StoryMapHome.propTypes = {

};
export default ({ ...props }) => (
  <DragDropContextProvider backend={HTML5Backend}>
    <StoryMapHome {...props} />
  </DragDropContextProvider>
);
