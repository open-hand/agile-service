import React, { useState, useReducer } from 'react';
import {
  WSHandler, stores,
  Choerodon,
} from '@choerodon/boot';
import _ from 'lodash';
import { Progress } from 'choerodon-ui/pro';
import { ProgressStatus, ProgressType } from 'choerodon-ui/lib/progress/enum';
import './index.less';

interface Props {
    // eslint-disable-next-line react/require-default-props
    downloadUrl?: string,
    messageKey: string,
}
interface StateProps {
    visible: false,
}

type ActionProps = Partial<StateProps> & { type: 'init' }
function WsProgress(props: Props) { // <StateProps, ActionProps>
  const [stateProgress, dispatch] = useReducer((state: StateProps, action: ActionProps) => {
    switch (action.type) {
      case 'init':
        return {
          ...state,
          visible: true,
        };
      default:
        return state;
    }
  }, {
    visible: false,
  });
  const [rate, setRate] = useState(0);
  const { messageKey } = props;
  function handleMessage(data: any) {
    dispatch({ type: 'init' });
  }

  function renderFinish() {
    const { downloadUrl } = props;
    return downloadUrl ? (
      <div className="c7n-agile-ws-finish">
        <span>导出完成时间2019-08-02 09:08（耗时1分钟）</span>
        <a href={downloadUrl}>点击下载</a>
      </div>
    ) : <></>;
  }
  //   return renderFinish();
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
          percent={rate}
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
