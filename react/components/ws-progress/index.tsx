/* eslint-disable react/require-default-props */
import React, {
  useState, useReducer, useEffect, useCallback,
} from 'react';
import {
  WSHandler, stores,
  Choerodon,
} from '@choerodon/boot';
import _ from 'lodash';
import { Progress } from 'choerodon-ui/pro';
import { ProgressStatus, ProgressType } from 'choerodon-ui/lib/progress/enum';
import './index.less';

interface Props {
  handleMessage?: (messageData: any) => void,
  downloadInfo?: { url: string, timeLine: string, children: React.ReactElement }
  renderEndProgress?: (messageData: any) => React.ReactElement | React.ReactElement[] | null | string,
  messageKey: string,
}
interface StateProps {
  visible: false,
  data: { rate: number, [propsName: string]: any },
}

type ActionProps = Partial<StateProps> & { type: 'init' | 'transmission' }
function WsProgress(props: Props) { // <StateProps, ActionProps>
  const [stateProgress, dispatch] = useReducer((state: StateProps, action: ActionProps) => {
    switch (action.type) {
      case 'init':
        return {
          ...state,
          data: action.data,
          visible: true,
        };
      default:
        return state;
    }
  }, {
    data: { rate: 0 },
    visible: false,
  });
  const { messageKey } = props;
  function handleMessage(data: any) {
    if (!stateProgress.visible && stateProgress.data?.rate === 0) {
      dispatch({ type: 'init', data });
    }
    dispatch({ type: 'transmission', data });
    props.handleMessage && props.handleMessage(data);
  }
  const renderFinish: any = useCallback(() => {
    const { downloadInfo, renderEndProgress } = props;

    if (renderEndProgress && typeof (props.renderEndProgress) === 'function') {
      // eslint-disable-next-line react-hooks/exhaustive-deps
      return renderEndProgress(stateProgress.data);
    }
    return downloadInfo ? (
      <div className="c7n-agile-ws-finish">
        {downloadInfo.children ?? (
        <>
          <span>{downloadInfo.timeLine}</span>
          <a href={downloadInfo.url}>点击下载</a>
        </>
        )}
        <span>{downloadInfo.timeLine}</span>

        {/* <span>导出完成时间2019-08-02 09:08（耗时1分钟）</span> */}
      </div>
    ) : <></>;
  }, [props.downloadInfo, stateProgress.data]);

  return stateProgress.visible ? (
    <WSHandler
      messageKey={messageKey}
      onMessage={handleMessage}
    >
      <div className="c7n-agile-ws-progress-area">
        <Progress
          className="c7n-agile-ws-progress"
          status={'active' as ProgressStatus}
          type={'circle' as ProgressType}
          width={50}
          percent={stateProgress.data?.rate}
          strokeWidth={16}
          showInfo={false}
        />
        <span className="c7n-agile-ws-progress-area-text">正在导入中</span>
        <span className="c7n-agile-ws-progress-area-prompt">（本次导入耗时较长，您可先返回进行其他操作）</span>
      </div>
    </WSHandler>
  ) : renderFinish();
}
export default WsProgress;
