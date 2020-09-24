/* eslint-disable react/require-default-props */
import React, {
  useState, useReducer, useEffect, useCallback,
} from 'react';
import {
  WSHandler, stores,
  Choerodon,
} from '@choerodon/boot';
import _ from 'lodash';
import fileSever, { FileSaverOptions } from 'file-saver';
import moment from 'moment';
import { Progress } from 'choerodon-ui/pro';
import { ProgressStatus, ProgressType } from 'choerodon-ui/lib/progress/enum';
import './index.less';
import { humanizeDuration } from '@/utils/common';
/**
 * @param fieldKey websocket传输信息下载url的key  默认fileUrl
 * @param fileName 下载文件名  默认为url 最后一个‘/’后的名
 * @param  fileSaverOptions
 */
interface AutoDownloadProps {
  fieldKey?: string,
  fileName?: string,
  fileSaverOptions?: FileSaverOptions,
}
interface Props {
  handleMessage?: (messageData: any) => void | boolean,
  percentCode?: string,
  downloadInfo?: {
    url: string,
    timeLine?: any,
    createDate?: string,
    lastUpdateDate?: string,
    timeFormat?: string,
    children?: React.ReactElement
  } | null,
  renderEndProgress?: (messageData: any) => React.ReactElement | React.ReactElement[] | null | string,
  visible?: boolean, // 可控类型 控制进度条是否显示
  messageKey: string,
  autoDownload?: boolean | AutoDownloadProps, /** 完成后是否自动下载 */
  onFinish?: (messageData: any) => void,
}
interface StateProps {
  visible: boolean,
  data: { [propsName: string]: any },
}
function onHumanizeDuration(createDate?: string, lastUpdateDate?: string): string | null {
  if (!createDate || !lastUpdateDate) {
    return null;
  }
  const startTime = moment(createDate);
  const lastTime = moment(lastUpdateDate);
  let diff = lastTime.diff(startTime);
  if (diff <= 0) {
    diff = moment().diff(startTime);
  }
  return humanizeDuration(diff);
}

type ActionProps = Partial<StateProps> & { type: 'init' | 'transmission' | 'visible' | 'finish' }
function WsProgress(props: Props) { // <StateProps, ActionProps>
  const { percentCode = 'process' } = props;
  const [stateProgress, dispatch] = useReducer((state: StateProps, action: ActionProps) => {
    switch (action.type) {
      case 'init':
        return {
          ...state,
          data: action.data,
          visible: true,
        };
      case 'visible':
        return {
          ...state,
          visible: action.visible,
        };
      case 'transmission':
        return {
          ...state,
          data: action.data,
        };
      case 'finish': {
        if (props.onFinish && typeof (props.onFinish) === 'function') {
          props.onFinish(action.data);
        }
        return {
          ...state,
          visible: false,
          data: action.data,
        };
      }

      default:
        return state;
    }
  }, {
    data: { [percentCode]: 0 },
    visible: false,
  });
  const { messageKey } = props;
  function handleFinish(data: any) {
    const { autoDownload } = props;
    dispatch({ type: 'finish', data });
    if (autoDownload) {
      const autoDownLoadFieldCode = typeof (autoDownload) === 'boolean' ? 'fileUrl' : autoDownload.fieldKey;
      const url = data[autoDownLoadFieldCode || 'fileUrl'];
      const fileName = url.substring(url.lastIndexOf('/') + 1);
      fileSever.saveAs(data[autoDownLoadFieldCode || 'fileUrl'], fileName);
    }
  }
  function handleMessage(data: any) {
    const newData = JSON.parse(data);
    const { status } = newData;
    if (!stateProgress.visible && stateProgress.data?.[percentCode] === 0) {
      props.handleMessage && props.handleMessage(newData);
      dispatch({ type: 'init', data: newData });
      return;
    }
    dispatch({ type: 'transmission', data: newData });
    if (props.handleMessage && props.handleMessage(newData)) {
      handleFinish(newData);
      return;
    }
    if (status && status === 'success') {
      handleFinish(newData);
    }
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
          downloadInfo.url
            ? (
              <>
                <span>{downloadInfo.timeLine ?? `导出完成时间${downloadInfo.lastUpdateDate}（耗时${onHumanizeDuration(downloadInfo.createDate, downloadInfo.lastUpdateDate)}）`}</span>
                <a href={downloadInfo.url}>点击下载</a>
              </>
            ) : ''
        )}
        {/* <span>导出完成时间2019-08-02 09:08（耗时1分钟）</span> */}
      </div>
    ) : <></>;
  }, [props.downloadInfo, stateProgress.data]);
  useEffect(() => {
    if (typeof (props.visible) !== 'undefined') {
      dispatch({ type: 'visible', visible: props.visible });
    }
  }, [props.visible]);
  return (
    <WSHandler
      messageKey={messageKey}
      onMessage={handleMessage}
    >
      {stateProgress.visible ? (
        <div className="c7n-agile-ws-progress-area">
          <Progress
            className="c7n-agile-ws-progress"
            status={'active' as ProgressStatus}
            type={'circle' as ProgressType}
            width={50}
            percent={stateProgress.data?.[percentCode]}
            strokeWidth={16}
            showInfo={false}
          />
          <span className="c7n-agile-ws-progress-area-text">正在导出中</span>
          <span className="c7n-agile-ws-progress-area-prompt">（本次导入耗时较长，您可先返回进行其他操作）</span>
        </div>
      ) : renderFinish()}
    </WSHandler>
  );
}
export default WsProgress;
export { onHumanizeDuration as calculateHumanizeDuration };
