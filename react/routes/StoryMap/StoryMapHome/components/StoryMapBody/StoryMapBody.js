import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { IsInProgram } from '@/hooks/useIsInProgram';
import EpicRow from './EpicRow';
import FeatureRow from './FeatureRow';
import StoryArea from './StoryArea';
import './StoryMapBody.less';

@observer
class StoryMapBody extends Component {
  render() {
    console.log('bodyrender');
    return (
      <div className="c7nagile-StoryMapBody">
        <table className="c7nagile-StoryMapBody-table">
          <tbody>
            <EpicRow />
            {/* 在项目群下才显示 */}
            <IsInProgram>
              {
                ({ isInProgram }) => (
                  isInProgram && <FeatureRow />
                )
              }
            </IsInProgram>
            <StoryArea />
            {/* <tr style={{ visibility: 'hidden', height: 'auto' }} /> */}
          </tbody>
        </table>
      </div>
    );
  }
}

StoryMapBody.propTypes = {

};

export default StoryMapBody;
