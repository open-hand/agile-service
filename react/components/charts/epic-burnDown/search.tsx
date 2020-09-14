import React from 'react';
import { Select, CheckBox } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import to, { linkUrl } from '@/utils/to';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import seeChangeRange from './image/seeChangeRange.svg';
import seeProgress from './image/seeProgress.svg';
import styles from './index.less';

export interface IEpic {
  issueId: string,
  epicName: string,
  issueNum: string,
  summary: string,
}
export interface EpicBurnDownSearchProps {
  epics: IEpic[]
  epicFinishLoading: boolean,
  checked: 'checked' | undefined,
  currentEpicId: string,
  setCurrentEpicId: Function,
  setChecked: Function,
}

const { Option } = Select;

const EpicBurnDownSearch:React.FC<EpicBurnDownSearchProps> = ({
  epics, epicFinishLoading, checked, currentEpicId, setCurrentEpicId, setChecked,
}) => {
  const handleChangeCurrentEpic = (epicId: string) => {
    setCurrentEpicId(epicId);
  };

  const handleCheckedChange = (value: 'checked' | undefined) => {
    setChecked(value);
  };

  const handleIconMouseEnter = () => {
    const iconShowInfo = document.getElementsByClassName('icon-show-info')[0];
    // @ts-ignore
    iconShowInfo.style.display = 'flex';
  };

  const handleIconMouseLeave = () => {
    const iconShowInfo = document.getElementsByClassName('icon-show-info')[0];
    // @ts-ignore
    iconShowInfo.style.display = 'none';
  };

  const handleLinkToIssue = (linkType: string, item: IEpic) => {
    if (JSON.stringify(item) !== '{}') {
      if (linkType === 'epic') {
        LINK_URL_TO.issueLinkTo(item.issueId, item.issueNum);
      }
      to(LINK_URL.workListIssue);
    }
  };

  const renderEpicInfo = () => {
    if (currentEpicId !== undefined) {
      const currentEpic = epics.find((item) => item.issueId === currentEpicId);
      return (
        <p className="c7n-epicInfo">
          <span
            className="primary"
            style={{
              cursor: 'pointer',
            }}
            role="none"
            onClick={handleLinkToIssue.bind(this, 'epic', currentEpicId !== undefined ? epics.filter((item) => item.issueId === currentEpicId)[0] : {})}
          >
            {`${currentEpic ? currentEpic.issueNum : ''}`}
          </span>
          <span>{` ${currentEpic ? currentEpic.summary : ''}`}</span>
        </p>
      );
    }
    return '';
  };

  return (
    <>
      {
      (epics.length > 0 && !epicFinishLoading) && (
        <div className={styles.epicBurnDown_search}>
          <div style={{ display: 'flex' }}>
            <Select
              style={{ width: 512, marginRight: 33, height: 35 }}
              label="史诗"
              value={currentEpicId}
              onChange={handleChangeCurrentEpic}
              // @ts-ignore
              getPopupContainer={((triggerNode) => triggerNode.parentNode)}
            >
              {
                epics.map((epic) => (
                  <Option key={epic.issueId} value={epic.issueId}>{epic.epicName}</Option>
                ))
              }
            </Select>
            <div className="c7n-epicSelectHeader">
              <CheckBox
                label="查看选项"
                value="checked"
                // @ts-ignore
                checked={checked?.indexOf('checked') > -1}
                onChange={handleCheckedChange}
              >
                根据图表校准冲刺
              </CheckBox>
              <span className="icon-show" role="none" onMouseEnter={handleIconMouseEnter} onMouseLeave={handleIconMouseLeave}>
                <Icon type="help icon" />
              </span>

              <div className="icon-show-info" onMouseEnter={handleIconMouseEnter} onMouseLeave={handleIconMouseLeave}>
                <figure className="icon-show-progress">
                  <div className="icon-show-info-svg">
                    <img src={seeProgress} alt="查看进度" />
                  </div>
                  <figcaption className="icon-show-info-detail">
                    <p className="icon-show-info-detail-header">查看进度</p>
                    <p className="icon-show-info-detail-content">按照史诗查看冲刺进度</p>
                  </figcaption>
                </figure>
                <figure>
                  <div className="icon-show-info-svg">
                    <img src={seeChangeRange} alt="查看变更范围" />
                  </div>
                  <figcaption className="icon-show-info-detail">
                    <p className="icon-show-info-detail-header">查看变更范围</p>
                    <p className="icon-show-info-detail-content">跟踪范围的扩大和缩小，由底部条状信息显示。</p>
                  </figcaption>
                </figure>
              </div>
            </div>
          </div>
          <div>
            {renderEpicInfo()}
          </div>
        </div>
      )
    }
    </>
  );
};

export default observer(EpicBurnDownSearch);
