import React, {
  useState, useReducer, useEffect, useCallback, useMemo,
} from 'react';
import { WSHandler, Choerodon } from '@choerodon/boot';
import _ from 'lodash';
import fileSever, { FileSaverOptions } from 'file-saver';
import moment from 'moment';
import { Progress } from 'choerodon-ui/pro';
import { ProgressStatus, ProgressType } from 'choerodon-ui/lib/progress/enum';
import './index.less';
import { humanizeDuration } from '@/utils/common';
import { observer } from 'mobx-react-lite';
/**
 * @param fieldKey websocket传输信息下载url的key  默认fileUrl
 * @param fileName 下载文件名  默认为url 最后一个‘/’后的名
 * @param  fileSaverOptions
 */
interface DownloadProps {
  fieldKey?: string,
  fileName?: string,
  fileSaverOptions?: FileSaverOptions,
}
interface Props {
  handleMessage?: (messageData: any) => void | boolean, /** 当回调返回true时代表websocket任务完成 */
  percentKey?: string, /** 在websocket 信息中进程属性的key 默认为 process */
  downloadInfo?: {
    url: string | null,
    timeLine?: any,
    createDate?: string,
    lastUpdateDate?: string,
    timeFormat?: string, /** 默认时间解析格式  YYYY-MM-DD HH:mm:ss */
    children?: React.ReactElement
  } | null,
  renderEndProgress?: (messageData: any) => React.ReactElement | React.ReactElement[] | null | string,
  visible?: boolean, /**  可控类型 控制进度条是否显示 */
  messageKey: string,
  autoDownload?: boolean | DownloadProps, /** 完成后是否自动下载 */
  downloadProps?: DownloadProps, /** 下载文件配置，当存在自动下载配置时，以自动下载配置为最高优先级 */
  onFinish?: (messageData: any) => void, /** websocket任务完成后回调 */
  onStart?: (messageData: any) => void, /** websocket任务开始时回调 */
}
interface StateProps {
  visible: boolean,
  data: { [propsName: string]: any },
  lastSuccessData: { [propsName: string]: any },
}
function onHumanizeDuration(createDate?: string, lastUpdateDate?: string, timeFormat: string = 'YYYY-MM-DD HH:mm:ss'): string | null {
  if (!createDate || !lastUpdateDate) {
    return null;
  }
  const startTime = moment(createDate, timeFormat);
  const lastTime = moment(lastUpdateDate, timeFormat);
  let diff = lastTime.diff(startTime);
  if (diff < 0) {
    diff = moment().diff(startTime);
  }
  return humanizeDuration(diff);
}

type ActionProps = Partial<StateProps> & { type: 'init' | 'transmission' | 'visible' | 'finish' }
const WsProgress: React.FC<Props> = (props) => { // <StateProps, ActionProps>
  const { percentKey = 'process' } = props;
  const downLoadProps = useMemo(() => {
    const tempProps = props.downloadProps;
    if (typeof (props.autoDownload) === 'object') {
      return { ...tempProps, ...props.autoDownload };
    }
    return tempProps;
  }, [props.downloadProps]);
  const [stateProgress, dispatch] = useReducer((state: StateProps, action: ActionProps) => {
    switch (action.type) {
      case 'init':
        if (props.onStart && typeof (props.onStart) === 'function') {
          props.onStart(action.data);
        }
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
          data: { [percentKey]: 0 },
          visible: false,
          lastSuccessData: action.data,
        };
      }

      default:
        return state;
    }
  }, {
    data: { [percentKey]: 0 },
    lastSuccessData: {},
    visible: false,
  });
  const { messageKey } = props;
  function handleFinish(data: any) {
    const { autoDownload } = props;
    dispatch({ type: 'finish', data });
    if (autoDownload) {
      const autoDownLoadFieldCode = typeof (autoDownload) === 'boolean' ? 'fileUrl' : autoDownload.fieldKey;
      const url = data[autoDownLoadFieldCode || 'fileUrl'];
      const fileName = downLoadProps?.fileName ?? url.substring(url.lastIndexOf('/') + 1);
      fileSever.saveAs(data[autoDownLoadFieldCode || 'fileUrl'], fileName, downLoadProps?.fileSaverOptions);
    }
  }
  function handleMessage(data: any) {
    const newData = JSON.parse(data);
    const { status } = newData;
    if (!stateProgress.visible && stateProgress.data?.[percentKey] === 0) {
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
    const fileName = downLoadProps?.fileName ?? downloadInfo?.url?.substring(downloadInfo.url.lastIndexOf('/') + 1);
    return downloadInfo ? (
      <div className="c7n-agile-ws-finish">
        {downloadInfo.children ?? (
          downloadInfo.url
            ? (
              <>
                <span>{downloadInfo.timeLine ?? `导出完成时间${downloadInfo.lastUpdateDate}（耗时${onHumanizeDuration(downloadInfo.createDate, downloadInfo.lastUpdateDate, downloadInfo.timeFormat)}）`}</span>
                <span role="none" className="c7n-agile-ws-finish-url" onClick={() => downloadInfo.url && fileSever.saveAs(downloadInfo.url, fileName, downLoadProps?.fileSaverOptions)}>点击下载</span>
              </>
            ) : ''
        )}
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
            percent={stateProgress.data?.[percentKey]}
            strokeWidth={16}
            showInfo={false}
          />
          <span className="c7n-agile-ws-progress-area-text">正在导出中</span>
          <span className="c7n-agile-ws-progress-area-prompt">（本次导入耗时较长，您可先返回进行其他操作）</span>
        </div>
      ) : renderFinish()}
    </WSHandler>
  );
};
export default observer(WsProgress);
export { onHumanizeDuration as calculateHumanizeDuration };
