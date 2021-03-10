interface IReleaseDetailData {
  id: string
  name: string
  startDate: string
  expectReleaseDate: string
  statusCode: 'version_planning' | 'released' | 'archived'
  statusName: string
  createDate: string
  actualReleaseDate: string
  creator: any
  description: string
}
export default IReleaseDetailData;
