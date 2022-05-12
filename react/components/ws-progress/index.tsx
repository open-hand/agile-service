import React, {
  useReducer, useEffect, useCallback, useMemo,
} from 'react';
import { WSHandler } from '@choerodon/boot';
import fileSever, { FileSaverOptions } from 'file-saver';
import moment from 'moment';
import { usePersistFn } from 'ahooks';
import { Button, Progress, Icon } from 'choerodon-ui/pro';
import { ProgressStatus, ProgressType } from 'choerodon-ui/lib/progress/enum';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import { humanizeDuration } from '@/utils/common';
import './index.less';
import DownLoad from '@/assets/icons/Download';
import Divider from '../EditIssue/IssueComponent/IssueBody/Divider';
import useFormatMessage from '@/hooks/useFormatMessage';
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
export interface IWsProgressProps {
  /** 当回调返回true时代表websocket任务完成 */
  handleMessage?: (messageData: Record<string, any>) => void | boolean,
  /** 在websocket 信息中进程属性的key 默认为 process */
  percentKey?: string,
  downloadInfo?: {
    url: string | null,
    timeLine?: any,
    createDate?: string,
    lastUpdateDate?: string,
    /** 默认时间解析格式  YYYY-MM-DD HH:mm:ss */
    timeFormat?: string,
    children?: React.ReactElement
  } | null,
  renderEndProgress?: (messageData?: Record<string, any>) => React.ReactElement | React.ReactElement[] | null | string,
  /**  可控类型 控制进度条是否显示 */
  visible?: boolean,
  messageKey: string,
  className?: string,
  /** 预定义配置 ws进行时的下方提示文字的配置 默认export */
  predefineProgressTextConfig?: 'export' | 'import' | 'none'
  /** 完成后是否自动下载 */
  autoDownload?: boolean | DownloadProps,
  /** 下载文件配置，当存在自动下载配置时，以自动下载配置为最高优先级 */
  downloadProps?: DownloadProps,
  /** websocket任务完成后回调 */
  onFinish?: (messageData: any) => void,
  /** websocket任务开始时回调 */
  onStart?: (messageData: any) => void,
  /** websocket任务失败时回调 */
  onFailed?: (messageData: any) => void,
  downloadBtn?: boolean
}
interface StateProps {
  visible: boolean,
  data: { [propsName: string]: any },
  lastData: { [propsName: string]: any },
  lastSuccessData: { [propsName: string]: any },
  lastFailedData: { [propsName: string]: any },
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

type ActionProps = Partial<StateProps> & { type: 'init' | 'transmission' | 'visible' | 'finish' | 'failed' }
const WsProgress: React.FC<IWsProgressProps> = observer((props) => { // <StateProps, ActionProps>
  const { percentKey = 'process', downloadBtn = false } = props;
  const formatMessage = useFormatMessage();

  const downLoadProps = useMemo(() => {
    const tempProps = props.downloadProps;
    if (typeof (props.autoDownload) === 'object') {
      return { ...tempProps, ...props.autoDownload };
    }
    return tempProps;
  }, [props.autoDownload, props.downloadProps]);
  const doingTextTemplate = useMemo(() => {
    if (props.predefineProgressTextConfig !== 'none') {
      switch (props.predefineProgressTextConfig) {
        case 'import':
          return formatMessage({ id: 'boot.import' });
        case 'export':
          return formatMessage({ id: 'boot.export' });
        default:
          return formatMessage({ id: 'boot.export' });
      }
    }
    return '';
  }, [formatMessage, props.predefineProgressTextConfig]);
  const onFinish = usePersistFn(props.onFinish || ((d: any) => { }));
  const onFailed = usePersistFn((d: any) => props.onFailed && props.onFailed(d));
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
          lastData: action.data,
          data: action.data,
        };
      case 'finish': {
        return {
          data: { [percentKey]: 0 },
          visible: false,
          lastData: action.data,
          lastSuccessData: action.data,
        };
      }
      case 'failed': {
        onFailed(action.data);
        return {
          data: { [percentKey]: 0 },
          visible: false,
          lastData: action.data,
          lastFailedData: action.data,
        };
      }

      default:
        return state;
    }
  }, {
    data: { [percentKey]: 0 },
    lastSuccessData: {},
    lastData: {},
    lastFailedData: {},
    visible: false,
  });
  const { messageKey } = props;
  const handleFinish = useCallback((data: any) => {
    dispatch({ type: 'finish', data });
    onFinish(data);
    if (props.autoDownload) {
      const autoDownLoadFieldCode = typeof (props.autoDownload) === 'boolean' ? 'fileUrl' : props.autoDownload.fieldKey;
      const url = data[autoDownLoadFieldCode || 'fileUrl'];
      const fileName = downLoadProps?.fileName ?? url.substring(url.lastIndexOf('/') + 1);
      fileSever.saveAs(data[autoDownLoadFieldCode || 'fileUrl'], fileName, downLoadProps?.fileSaverOptions);
    }
  }, [downLoadProps?.fileName, downLoadProps?.fileSaverOptions, onFinish, props.autoDownload]);
  const handleMessage = useCallback((data: any) => {
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
    if (status && status === 'failed') {
      dispatch({ type: 'failed', data: newData });
    }
  }, [handleFinish, percentKey, props, stateProgress.data, stateProgress.visible]);
  const renderFinish: any = useCallback(() => {
    const { downloadInfo, renderEndProgress } = props;
    if (renderEndProgress && typeof (props.renderEndProgress) === 'function') {
      // eslint-disable-next-line react-hooks/exhaustive-deps
      return renderEndProgress(stateProgress.lastData);
    }
    const fileName = downLoadProps?.fileName ?? downloadInfo?.url?.substring(downloadInfo.url.lastIndexOf('/') + 1);
    return downloadInfo ? (
      <div>
        <Divider />
        <div style={{ fontWeight: 500, fontSize: '14px', marginBottom: 14 }}>历史记录</div>
        <div className="c7n-agile-ws-finish">
          {downloadInfo.children ?? (
            downloadInfo.url
              ? (
                <>
                  <span>{downloadInfo.timeLine ?? `${doingTextTemplate}完成时间${downloadInfo.lastUpdateDate}（耗时${onHumanizeDuration(downloadInfo.createDate, downloadInfo.lastUpdateDate, downloadInfo.timeFormat)}）`}</span>
                  {
                    downloadBtn ? (
                      <Button icon="archive" className="c7n-agile-ws-progress-download-btn" onClick={() => downloadInfo.url && fileSever.saveAs(downloadInfo.url, fileName, downLoadProps?.fileSaverOptions)}>下载历史</Button>
                    ) : (
                      <span role="none" className="c7n-agile-ws-finish-url" onClick={() => downloadInfo.url && fileSever.saveAs(downloadInfo.url, fileName, downLoadProps?.fileSaverOptions)}>
                        下载历史
                        <DownLoad style={{ marginLeft: 6 }} />
                      </span>
                    )
                  }
                </>
              ) : ''
          )}
        </div>
      </div>
    ) : <></>;
  }, [doingTextTemplate, downLoadProps?.fileName, downLoadProps?.fileSaverOptions, downloadBtn, props, stateProgress.lastData]);
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
        <div className={classnames('c7n-agile-ws-progress-area', props.className)}>
          <Progress
            className="c7n-agile-ws-progress"
            status={'active' as ProgressStatus}
            type={'circle' as ProgressType}
            width={50}
            percent={stateProgress.data?.[percentKey]}
            strokeWidth={16}
            showInfo={false}
          />
          <span className="c7n-agile-ws-progress-area-text">{`正在${doingTextTemplate}中`}</span>
          <span className="c7n-agile-ws-progress-area-prompt">
            （本次
            {doingTextTemplate}
            耗时较长，您可先返回进行其他操作）
          </span>
        </div>
      ) : renderFinish()}
    </WSHandler>
  );
});
export default WsProgress;
export { onHumanizeDuration as calculateHumanizeDuration };
