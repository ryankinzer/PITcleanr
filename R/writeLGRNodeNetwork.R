#'@title LGR Node Network
#'
#'@description Create data.frame describing all the observation nodes in the
#'  Lower Granite dam version of DABOM, including how they are related to one
#'  another. Modified on 1-15-19 to include additonal sites and branches for
#'  DABOM spawn year 2019
#'
#'@author Kevin See
#'
#'@import dplyr stringr
#'@export
#'@return NULL
#' @examples writeLGRNodeNetwork()

writeLGRNodeNetwork = function() {
  bin_names = c('Below_GRA',
                'Clearwater',
                'Grande_Ronde',
                'Imnaha',
                'Salmon',
                'Snake')

  bin_list = vector('list', length(bin_names))
  names(bin_list) = bin_names

  bin_list[['Below_GRA']] = list(
    'Tucannon' =
      list('LTR' =
             list(
               'LTR',
               'TUCR',
               'MTR' =
                 list('MTR',
                      'PATAHC',
                      'UTR' =
                        list('UTR',
                             'TUCH',
                             'TFH',
                             'CURP'))
             )),
    'Penawawa' = list('PENAWC'),
    'Almota' = list('ALMOTC'),
    'Below_GRA_Other' = list('Below_GRA_Obs')
  )

  bin_list[['Clearwater']] = list(
    'Lapwai' = list('LAP' =
                      list(
                        'LAP',
                        'LAPC',
                        'MIS' =
                          list('MIS',
                               'MISSC'),
                        'SWT' =
                          list('SWT',
                               'SWEETC',
                               'WEB' =
                                 list('WEB',
                                      'WEBBC'))
                      )),
    'Potlatch' = list(
      'JUL' =
        list(
          'JUL',
          'BOBSG',
          'BOUL3C',
          'CEDA2C',
          'CORRAC',
          'LBOULC',
          'LEOPOC',
          'MPOTLC',
          'PIVASC',
          'POTR',
          'POTREF',
          'KHS' =
            list('KHS',
                 'LBEARC',
                 'LBCWF',
                 'BBA' =
                   list('BBA',
                        'BIGBEC')),
          'HLM' =
            list('HLM',
                 'COUG2C',
                 'FEATHC',
                 'MOOS3C',
                 'POTRWF',
                 'PURDUC'),
          'PCM' =
            list('PCM',
                 'PINE2C')
        )),
    'Lolo' = list('LC1' =
                    list('LC1',
                         'LC2' =
                           list(
                             'LC2',
                             'ELDORC',
                             'LOLOC'
                           ))),
    'SFClearwater' = list('SC1' =
                            list(
                              'SC1',
                              'SC2' =
                                list(
                                  'SC2',
                                  'AMERR',
                                  'CROOKP',
                                  'CROOKR',
                                  'CROP',
                                  'CROTRP',
                                  'CROW',
                                  'CRT',
                                  'FIVEMC',
                                  'JOHNC',
                                  'LUGUAF',
                                  'MEAD2C',
                                  'MILL2C',
                                  'NEWSOC',
                                  'REDP',
                                  'REDR',
                                  'REDRSF',
                                  'REDTRP',
                                  'RELIEC',
                                  'RRT',
                                  'TENMIC',
                                  'TWNMIC',
                                  'TWNMIT'
                                )
                            )),
    'ClearCreek' = list('CLC' =
                          list('CLC',
                               'KOOS',
                               'CLEARC')),
    'Selway' = list(
      'SW1' =
        list(
          'SW1',
          'CEFLAF',
          'OHARAC',
          'SW2' =
            list(
              'SW2',
              '3LINKC',
              'BEARC',
              'EAGLEC',
              'GEDCWF',
              'GEDNEC',
              'MEADOC',
              'MINKC',
              'MOOS2C',
              'MOOS2N',
              'RUNNIC',
              'SELWY2',
              'WHITCC'
            )
        )),
    'Lochsa' = list('LRL' =
                      list(
                        'LRL',
                        'LRU' =
                          list(
                            'LRU',
                            'BIGFLC',
                            'BOULDC',
                            'BOUTRP',
                            'BRUSHC',
                            'CANYOC',
                            'CFCTRP',
                            'COLTC',
                            'COLTKC',
                            'CROOKC',
                            'DEADMC',
                            'OLDMAC',
                            'PAPOOC',
                            'PETEKC',
                            'POSTOC',
                            'POWP',
                            'SQUAWC',
                            'STORMC',
                            'WARMSC',
                            'WFITSC',
                            'FISTRP' =
                              list('FISTRP',
                                   'FISHC',
                                   'HUNGC')
                          )
                      )),
    'Clearwater_Other' = list(
      'BCCAP',
      'BEDRKC',
      'BIGCAC',
      'CLJ',
      'CLWH',
      'CLWHNF',
      'CLWR',
      'CLWRMF',
      'CLWRNF',
      'CLWRSF',
      'CLWTRP',
      'COTNWC',
      'DWL',
      'DWOR',
      'DWORMS',
      'DWORNF',
      'JACKSC',
      'LITCAC',
      'NPTH',
      'OROFC',
      'SWSP'
    )
  )

  bin_list[['Snake']] = list(
    'Asotin' = list('ACM' =
                      list(
                        'ACM',
                        'GEORGC',
                        'ASOTIC' =
                          list('ASOTIC',
                               'ACB' =
                                 list(
                                   'ACB',
                                   'AFC' =
                                     list('AFC',
                                          'ASOTNF',
                                          'ASOTSF'),
                                   'CCA' =
                                     list('CCA',
                                          'CHARLC')
                                 ))
                      )),
    'Alpowa' = list('ALPOWC'),
    'TenMileCreek' = list('TENMC2'),
    'Snake_Other' = list(
      'CATHEC',
      'CJRAP',
      'COTP',
      'COUGRC',
      'CTWD3C',
      'GRAND1',
      'GRAND2',
      'GRNTRP',
      'HCD',
      'HCDTAL',
      'IMJ',
      'IMNAHR',
      'IMNTRP',
      'OXBO',
      'PLAP',
      'SNAKE3',
      'SNAKE4',
      'SNJ',
      'SNKTRP',
      'WALLOR'
    )
  )

  bin_list[['Grande_Ronde']] = list(
    'Joseph_Creek' = list('JOC' =
                            list('JOC',
                                 'JOSEPC')),
    'Wenaha' =
      list('WEN' =
             list('WEN',
                  'WENR',
                  'WENRNF',
                  'WENRSF')),
    'LookingGlass' =
      list('LOOKGC',
           'LOOH'),
    'Upper_Grande_Ronde' =
      list('UGR' =
             list('UGR',
                  'UGS' =
                    list('UGS',
                         'GRANDW'))),
    'CCW' =
      list(
        'CCW',
        'CATHEW',
        'CATHEW',
        'CATCMF',
        'CATCNF',
        'CATCSF',
        'CATHEP',
        'CCP',
        'LCATHC'
      ),
  'Wallowa' =
    list(
      'WR1' =
        list('WR1',
             'BCANF',
             'WR2' =
               list('WR2',
                    'WALH',
                    'LOSTIW' =
                      list('LOSTIW',
                           'LOSTIR')),
             'MR1' =
               list('MR1',
                    'MINAMR')))
  )

bin_list[['Imnaha']] = list('Cow_Creek' =
                              list('COC' =
                                     list('COC',
                                          'COWRC')),
                            'ImnahaRiver' =
                              list('IR1' =
                                     list('IR1',
                                          'LITNGC',
                                          'IR2' =
                                            list('IR2',
                                                 'CMP' =
                                                   list('CMP',
                                                        'CAMP4C'),
                                                 'BSC' =
                                                   list('BSC',
                                                        'BSHEEC',
                                                        'LICK2C',
                                                        'SALTC'),
                                                 'LSHEEF' =
                                                   list('LSHEEF',
                                                        'LSHEEC',
                                                        'MCCULL',
                                                        'CANALC',
                                                        'REDMOC'),
                                                 'HORS3C',
                                                 'IR3' =
                                                   list('IR3',
                                                        'CZY' =
                                                          list('CZY',
                                                               'CRAZYC'),
                                                        'FREEZC',
                                                        'MAHOGC',
                                                        'IR4' =
                                                          list('IR4',
                                                               'IML' =
                                                                 list('IML',
                                                                      'IMNAHW' =
                                                                        list('IMNAHW',
                                                                             'IR5' =
                                                                               list('IR5',
                                                                                    'GUMBTC',
                                                                                    'DRY2C'))
                                                                 ))
                                                   )
                                            )
                                     )))

bin_list[['Salmon']] = list(
  'Rapid_River' =
    list('RAPH',
         'RPDTRP',
         'RAPIWF',
         'RPJ'),
  'SF_Salmon' =
    list(
      'SFG' =
        list('SFG',
             'ELK2C',
             'FITSUC',
             'LSFTRP',
             'SAEFSF',
             'SFSRKT',
             'ZEN' =
               list('ZEN',
                    'ALEXC',
                    'FLATC',
                    'GROUSC',
                    'LAKEC',
                    'LICKC',
                    'PIAHC',
                    'RUBYC',
                    'SECTRP',
                    'SUMITC',
                    'ZENAC',
                    'ZENAWF'
               ),
             'KRS' =
               list('KRS',
                    'BCKHRC',
                    'CAMP3C',
                    'GOATC',
                    'PHOEBC',
                    'SALRSF',
                    'STR' =
                      list('STR',
                           'SFSTRP',
                           'KNOXB',
                           'MCCA',
                           'SALSFW'=
                             list('SALSFW',
                                  'BEAR4C',
                                  'RICEC',
                                  'STOLP')
                      )
               ),
             'ESS' =
               list('ESS',
                    'SUGARC',
                    'YPP' =
                      list('YPP',
                           'YPPL'),
                    'JOHTRP',
                    'JOHNSC' =
                      list('JOHNSC',
                           'BURNLC')
               )
        )),
  'Big_Creek' =
    list('TAY' =
           list('TAY',
                'BEAV4C',
                'BIG2C',
                'BIG2CT',
                'BRAMYC',
                'BUCK2C',
                'CABINC',
                'CAVEC',
                'CROO2C',
                'LOGANC',
                'MONCWF',
                'MONUMC',
                'RUSHC',
                'RUSHWF',
                'SMITHC',
                'SNOSLC'
           )
    ),
  'MFSR_Other' =
    list('BEAVC',
         'BOUNDC',
         'CAMASC',
         'CAPEHC',
         'DAGGEC',
         'ELKC',
         'FALLC',
         'INDIAC',
         'KNAPPC',
         'LOONC',
         'MARSHC',
         'MARTRP',
         'MARTRP',
         'PISTOC',
         'RAPR',
         'SALMF1',
         'SALMF2',
         'SALRMF',
         'SHEPC',
         'SULFUC',
         'WILSOC',
         'YELLJC'),
  'Bear_Valley' =
    list('BRC' =
           list('BRC',
                'BEARVC')
    ),
  'Carmen_Creek' =
    list('CRC',
         list('CRC',
              'CARMEC')
    ),
  'Panther_Creek' =
    list('PCA' =
           list('PCA',
                'MOYERC',
                'MUSCRC',
                'PANTHC')
    ),
  'NFSalmon' =
    list('NFS' =
         list('NFS',
         'SALRNF')),
'Lemhi' =
  list(
    'LLR' =
      list(
        'LLR',
        'HAYNSC',
        'LEMHIR',
        'LLRTP',
        'MCDEVC',
        'PATTEC',
        'PRATTC',
        'S2I',
        'S2O',
        'WITHGC',
        'BHC' =
          list('BHC',
               'BOHANC',
               'BOHEFC'),
        'WPC' =
          list('WPC',
               'WIMPYC'),
        'KEN' =
          list('KEN',
               'KENYC'),
        'AGC' =
          list('AGC',
               'AGNCYC',
               'COW2C',
               'FLUMEC'),
        'HYC' =
          list(
            'HYC',
            'BASINC',
            'BUCK4C',
            'BVAL2C',
            'HAYDEF',
            'HAYDNC',
            'HYDTRP',
            'TRAILC',
            'WRIGTC'
          ),
        'LRW' =
          list(
            'LRW',
            'BIGB2C',
            'DEERC',
            'LEMHIW',
            'LEMTRP',
            'LIT8MC',
            'MILL5C',
            'QKASPC',
            'RESVRC',
            'YRIANC',
            'LLS' =
              list('LLS',
                   'LLSPRC'),
            'LB8' =
              list('LB8',
                   'BIG8MC'),
            'LBS' =
              list('LBS',
                   'BIGSPC'),
            'LCL' =
              list('LCL',
                   'LEEC'),
            'BTC' =
              list('BTC',
                   'BTL' =
                     list(
                       'BTL',
                       'BTIMBC',
                       'BTM' =
                         list('BTM',
                              'LTIMBC',
                              'BTU' =
                                list('BTU',
                                     'BASN2C'))
                     )),
            'CAC' =
              list('CAC',
                   'CANY2C',
                   'CRUIKS',
                   'WILDCC'),
            'HEC' =
              list('HEC',
                   'HAWLYC'),
            '18M' =
              list('18M',
                   '18MILC',
                   'TEXASC')
          ))),

'Upper_Salmon' = list(
  'USE' =
    list(
      'USE',
      'USI' =
        list(
          'USI',
          'CHALLC',
          'ELK3C',
          'HERDC',
          'IRONC',
          'POISNC',
          'REDFL',
          'REDFLC',
          'RFL',
          'RLCTRP',
          'SALR4',
          'SLAT2C',
          'SQAW2C',
          'SQUAWP',
          'STANLC',
          'PAHSIW' =
            list('PAHSIW',
                 'PAHH',
                 'PAHP',
                 'PAHTRP'),
          'SALEFT' =
            list('SALEFT',
                 'SALEFW'),
          'YFK' =
            list('YFK',
                 'YANKFK',
                 'CEY',
                 'BASN3C',
                 'STANLE',
                 'YANKWF'),
          'VC2' =
            list('VC2',
                 'VC1',
                 'VALEYC'),
          'STL' =
            list('STL',
                 'SAWT' =
                   list('SAWT',
                        '4JULYC',
                        'ALTULC',
                        'ALTURL',
                        'BEAVEC',
                        'CHAMPC',
                        'DECKEC',
                        'FISHEC',
                        'FRENCC',
                        'GOLDC',
                        'HELLRC',
                        'HUCKLC',
                        'PETTL',
                        'PETTLC',
                        'POLEC',
                        'SAWTRP',
                        'SMILEC',
                        'VATC',
                        'WILLIC',
                        'YELLLC'
                   )
            )
        ))),
'Salmon_Other' = list(
  '4JULYC',
  'BARGAC',
  'BIGMAC',
  'BOUL2C',
  'CHAMBC',
  'CHAMWF',
  'CROOC',
  'FLOSSC',
  'FRENCH',
  'HARDC',
  'HAZARC',
  'HORSEC',
  'JERSEC',
  'LSALR',
  'MOOSEC',
  'NFSTRP',
  'PAHSIR',
  'PARTRC',
  'RAPIDR',
  'SABEC',
  'SAJ',
  'SALR1',
  'SALR2',
  'SALR3',
  'SALREF',
  'SALTRP',
  'SHEEPC',
  'SLATEC',
  'TOWERC',
  'WBIRDC',
  'WINDR'
)
)


bin_all = list(
  'GRA' =
    list(
      'GRA',
      'Below_GRA' = bin_list[['Below_GRA']],
      'Clearwater' = bin_list[['Clearwater']],
      'Grande_Ronde' = bin_list[['Grande_Ronde']],
      'Imnaha' = bin_list[['Imnaha']],
      'Salmon' = bin_list[['Salmon']],
      'Snake' = bin_list[['Snake']]
    )
)

site_df_init = tibble(SiteID = unlist(bin_all),
                      path = names(unlist(bin_all))) %>%
  mutate(
    path = stringr::str_replace(path,
                                '[[:digit:]]$',
                                ''),
    path = stringr::str_replace(path,
                                'SC$',
                                'SC1'),
    path = ifelse(SiteID == 'LC1',str_replace(path,'LC','LC1'),path),
    path = stringr::str_replace(path,
                                'USI1',
                                'USI'),
    path = stringr::str_replace(path,
                                'LRW1',
                                'LRW'),
    path = stringr::str_replace(path,
                                'JUL[0-9]',
                                'JUL'),
    path = stringr::str_replace(path,
                                'SC2[0-9]$',
                                'SC2'),
    path = stringr::str_replace(path,
                                'SW21',
                                'SW2'),
    path = stringr::str_replace(path,
                                'LRU[0-9]',
                                'LRU'),
    path = stringr::str_replace(path,
                                'Clearwater_Other[0-9]',
                                'Clearwater_Other'),
    path = stringr::str_replace(path,
                                'BSC[0-9]',
                                'BSC'),
    path = stringr::str_replace(path,
                                'IR21',
                                'IR2'),
    path = stringr::str_replace(path,
                                'ZEN[0-9]',
                                'ZEN'),
    path = stringr::str_replace(path,
                                'JOHNSC[0-9]',
                                'JOHNSC'),
    path = stringr::str_replace(path,
                                'TAY[0-9]',
                                'TAY'),
    path = stringr::str_replace(path,
                                'SAWT[0-9]',
                                'SAWT'),
    path = stringr::str_replace(path,
                                'LLR[0-9]',
                                'LLR'),
    path = stringr::str_replace(path,
                                'Salmon_Other[0-9]',
                                'Salmon_Other'),
    path = stringr::str_replace(path,
                                'MFSR_Other[0-9]',
                                'MFSR_Other'),
    path = stringr::str_replace(path,
                                'Snake_Other[0-9]',
                                'Snake_Other'),
    path = ifelse(
      SiteID %in% c('IR1', 'IR4'),
      stringr::str_replace(path,
                           'IR$',
                           SiteID),
      path)) %>%
  mutate(path = ifelse(
    stringr::str_sub(path, start = -nchar(SiteID)) != SiteID,
    paste(path, SiteID, sep = '.'),
    path
  ))

network_descrip = stringr::str_split(site_df_init$path,
                                     '\\.',
                                     simplify = T)
colnames(network_descrip) = paste0('Step', 1:ncol(network_descrip))

site_df = site_df_init %>%
  bind_cols(network_descrip %>%
              as.data.frame()) %>%
  mutate_at(vars(matches('^Step')),
            funs(as.character)) %>%
  mutate(SiteID = factor(SiteID,
                         levels = unique(
                           site_df_init$SiteID
                         ))) %>%
  arrange(SiteID)


return(site_df)
}
