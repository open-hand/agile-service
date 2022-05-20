import React from 'react';
import { Select, CheckBox, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import seeChangeRange from './image/seeChangeRange.svg';
import seeProgress from './image/seeProgress.svg';
import styles from './index.less';
import { IChartSearchAdditionalProps } from '../types.';

export interface IVersion {
 versionId: string,
 name: string,
 releaseDate: null | string,
}
export interface VersionBurnDownSearchProps extends IChartSearchAdditionalProps{
  versions: IVersion[]
  versionIsLoading: boolean,
  checked: 'checked' | undefined,
  currentVersionId: string,
  setCurrentVersionId: Function,
  setChecked: Function,
  projectId?: string,
}

const { Option } = Select;

const VersionBurnDownSearch:React.FC<VersionBurnDownSearchProps> = ({
  versions, versionIsLoading, checked, currentVersionId, setCurrentVersionId, setChecked, searchDataSet,
}) => {
  const handleChangeCurrentVersion = (versionId: string) => {
    setCurrentVersionId(versionId);
  };

  const handleCheckedChange = (value: 'checked' | undefined) => {
    setChecked(value);
  };

  const handleIconMouseEnter = () => {
    const iconShowInfo = document.getElementsByClassName(styles.icon_show_info)[0];
    // @ts-ignore
    iconShowInfo.style.display = 'flex';
  };

  const handleIconMouseLeave = () => {
    const iconShowInfo = document.getElementsByClassName(styles.icon_show_info)[0];
    // @ts-ignore
    iconShowInfo.style.display = 'none';
  };

  const renderVersionInfo = () => {
    if (currentVersionId !== undefined) {
      const currentVersion = versions.filter((item) => item.versionId === currentVersionId)[0];
      return (
        <p className="c7n-versionInfo">
          {`${currentVersion && currentVersion.releaseDate === null ? '未发布' : (`发布于 ${currentVersion && currentVersion.releaseDate && currentVersion.releaseDate.split(' ')[0]}`)}`}
        </p>
      );
    }
    return '';
  };

  return (
    <>
      {
      (versions.length > 0 && !versionIsLoading) && (
        <div className={styles.epicBurnDown_search}>
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <Select
              className={styles.epicBurnDown_search_select}
              labelLayout={'float' as LabelLayout}
              clearButton={false}
              style={{ width: 500, marginRight: 33 }}
              label="版本"
              name="version"
              dataSet={searchDataSet}
              value={currentVersionId}
              onChange={handleChangeCurrentVersion}
            >
              {
                versions.map((version) => (
                  <Option
                    key={version.versionId}
                    value={version.versionId}
                  >
                    {version.name}
                  </Option>
                ))
              }
            </Select>
            <div className={styles.epicSelect}>
              <CheckBox
                style={{ marginTop: 15 }}
                label="查看选项"
                value="checked"
                checked={checked === 'checked'}
                onChange={handleCheckedChange}
              >
                根据图表校准冲刺
              </CheckBox>
              <span className={styles.icon_show} role="none" onMouseEnter={handleIconMouseEnter} onMouseLeave={handleIconMouseLeave}>
                <Icon type="help icon" />
              </span>

              <div className={styles.icon_show_info} onMouseEnter={handleIconMouseEnter} onMouseLeave={handleIconMouseLeave}>
                <figure className={styles.icon_show_progress}>
                  <div className={styles.icon_show_info_svg}>
                    <img src={seeProgress} alt="查看进度" />
                  </div>
                  <figcaption className={styles.icon_show_info_detail}>
                    <p className={styles.icon_show_info_detail_header}>查看进度</p>
                    <p className={styles.icon_show_info_detail_content}>按照版本查看冲刺进度</p>
                  </figcaption>
                </figure>
                <figure>
                  <div className="icon_show_info_svg">
                    <img src={seeChangeRange} alt="查看变更范围" />
                  </div>
                  <figcaption className={styles.icon_show_info_detail}>
                    <p className={styles.icon_show_info_detail_header}>查看变更范围</p>
                    <p className={styles.icon_show_info_detail_content}>跟踪范围的扩大和缩小，由底部条状信息显示。</p>
                  </figcaption>
                </figure>
              </div>
            </div>
          </div>
          <div>
            {renderVersionInfo()}
          </div>
        </div>
      )
    }
    </>
  );
};

export default observer(VersionBurnDownSearch);
