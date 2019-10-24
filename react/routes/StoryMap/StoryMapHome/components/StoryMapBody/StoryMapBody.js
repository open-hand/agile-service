import React, { Component } from 'react';
import { observer } from 'mobx-react';
import EpicRow from './EpicRow';
import StoryArea from './StoryArea';
import './StoryMapBody.less';

@observer
class StoryMapBody extends Component {
  render() {
    return (
      <div className="c7nagile-StoryMapBody">
        <table>
          <tbody>
            <EpicRow />
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
